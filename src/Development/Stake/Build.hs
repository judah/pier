{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Development.Stake.Build
    ( buildPackageRules
    , askBuiltPackages
    )
    where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup
import GHC.Generics hiding (packageName)
import Control.Monad (void)
import qualified Data.HashMap.Strict as HM
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Stake.Core
import Development.Stake.Package
import Development.Stake.Stackage
import Development.Stake.Witness
import Distribution.Compiler
import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.System (buildOS, buildArch)
import Distribution.Verbosity (normal)
import Distribution.Version (withinRange, Version(..))
import qualified Data.HashSet as HS
import Language.Haskell.Extension

{-
Outline:

downloads/stackage/plan/*.yaml
downloads/hackage/split-1.2.3 for downloaded and unpacked
build/lts-9.6/split/
    bin/{executable-name}
    pkgdb/package.conf
    pkgdb/**/*.hi
    pkgdb/*.a

-}

{-
Plan changes shared between snapshots:
built package gets a hash
and asking for one involves passing all the flags and deps
that it got, including their hashes


-}

instance Hashable FlagName

buildPackageRules :: Rules ()
buildPackageRules = do
    addWitness buildPackage
    pathsModuleRule

data ResolvePackageO = ResolvePackageO PlanName PackageName
    deriving (Show,Typeable,Eq,Generic)

instance Hashable ResolvePackageO
instance Binary ResolvePackageO
instance NFData ResolvePackageO
type instance RuleResult ResolvePackageO = Resolved

data BuiltPackageR = BuiltPackageR PlanName Resolved
    deriving (Show,Typeable,Eq,Generic)
instance Hashable BuiltPackageR
instance Binary BuiltPackageR
instance NFData BuiltPackageR
type instance RuleResult BuiltPackageR = BuiltPackage

-- ghc --package-db .stake/...text-1234.pkg/db --package text-1234
data BuiltPackage = BuiltPackage
                        { builtTransitiveDBs :: HS.HashSet FilePath
                        , builtPackageName :: PackageName
                        }
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)

askBuiltPackages :: PlanName -> [PackageName] -> Action [BuiltPackage]
askBuiltPackages planName pkgs = do
    plan <- askBuildPlan planName
    askWitnesses
        $ map (BuiltPackageR planName . resolvePackage plan)
        $ pkgs

buildPackage :: BuiltPackageR -> Action BuiltPackage
buildPackage (BuiltPackageR plan r) = do
    rerunIfCleaned
    buildResolved plan r

buildResolved :: PlanName -> Resolved -> Action BuiltPackage
buildResolved _ (Resolved Builtin p) = do
    return BuiltPackage { builtTransitiveDBs = HS.empty
                        , builtPackageName = packageName p
                        }
buildResolved planName (Resolved Additional p) = do
    -- TODO: what if the .cabal file has a different basename?
    -- Maybe make this a witness too.
    let f = packageSourceDir p </> unPackageName (pkgName p) <.> "cabal"
    need [f]
    gdesc <- liftIO $ readPackageDescription normal f
    plan <- askBuildPlan planName
    desc <- flattenToDefaultFlags plan gdesc
    buildFromDesc planName plan desc

packageSourceDir :: PackageId -> FilePath
packageSourceDir pkg = artifact $ "downloads/hackage" </> display pkg

flattenToDefaultFlags
    :: BuildPlan -> GenericPackageDescription -> Action PackageDescription
flattenToDefaultFlags plan gdesc = do
    let desc0 = packageDescription gdesc
    let flags = HM.fromList [(flagName f, flagDefault f)
                        | f <- genPackageFlags gdesc
                        ]
    return desc0 {
        -- TODO: Nothing vs Nothing?
        library = fmap (resolve plan flags) $ condLibrary gdesc
       }

resolve
    :: Semigroup a
    => BuildPlan
    -> HM.HashMap FlagName Bool
    -> CondTree ConfVar [Dependency] a
    -> a
resolve plan flags node
    = sconcat
        $ condTreeData node :|
        [ resolve plan flags t
        | (cond,ifTrue,ifFalse) <- condTreeComponents node
        , Just t <- [if isTrue plan flags cond
                        then Just ifTrue
                        else ifFalse]]

isTrue :: BuildPlan -> HM.HashMap FlagName Bool -> Condition ConfVar -> Bool
isTrue plan flags = loop
  where
    loop (Var (Flag f))
        | Just x <- HM.lookup f flags = x
        | otherwise = error $ "Unknown flag: " ++ show f
    loop (Var (Impl GHC range)) = withinRange (ghcVersion plan) range
    loop (Var (Impl _ _)) = False
    loop (Var (OS os)) = os == buildOS
    loop (Var (Arch arch)) = arch == buildArch
    loop (Lit x) = x
    loop (CNot x) = not $ loop x
    loop (COr x y) = loop x || loop y
    loop (CAnd x y) = loop x && loop y

buildFromDesc :: PlanName -> BuildPlan -> PackageDescription -> Action BuiltPackage
buildFromDesc planName plan desc
    | Just lib <- library desc
    , let lbi = libBuildInfo lib
    , buildable lbi = do
            let deps = [n | Dependency n _ <- targetBuildDepends
                                                lbi]
            builtDeps <- askBuiltPackages planName deps
            putNormal $ "Building " ++ display (package desc)
            buildLibrary planName plan builtDeps desc lib
    | otherwise = error "buildFromDesc: no library"

buildLibrary
    :: PlanName -> BuildPlan -> [BuiltPackage]
    -> PackageDescription -> Library
    -> Action BuiltPackage
buildLibrary planName plan deps desc lib
  | null (exposedModules lib) = return BuiltPackage
                { builtTransitiveDBs =
                        foldMap builtTransitiveDBs deps
                , builtPackageName = packageName $ package desc
                }

  | otherwise = do
    let lbi = libBuildInfo lib
    let pkgDir = (packageSourceDir (package desc) </>)
    let sourceDirs = (\ss -> if null ss then [pkgDir "."] else ss)
                        $ map pkgDir
                        $ hsSourceDirs lbi
    let buildDir = packageBuildDir planName (package desc)
    liftIO $ removeFiles buildDir ["//*"]
    let hiDir = buildDir </> "hi"
    let oDir = buildDir </> "o"
    modules <- mapM (findModule buildDir (packageName $ package desc)
                            sourceDirs)
                    $ otherModules lbi ++ exposedModules lib
    -- TODO: Actual LTS version ghc.
    -- TODO: dump output only if the command fails.
    command_ [] "ghc" $
        [ "-ddump-to-file"
        , "-this-unit-id", display $ package desc
        , "-hide-all-packages"
        , "-i"
        , "-hidir", hiDir
        , "-odir", oDir
        ]
        ++
        concat (map (\p -> ["-package-db", p])
                $ HS.toList
                $ foldMap builtTransitiveDBs deps)
        ++
        concat [["-package", display $ builtPackageName d]
                | d <- deps]
        ++ map ("-i"++) (sourceDirs ++ [buildDir </> "paths"])
        ++ map ("-I"++) (map pkgDir $ includeDirs lbi)
        ++ map ("-X" ++)
            (display (fromMaybe Haskell2010 $ defaultLanguage lbi)
            : map display (defaultExtensions lbi ++ oldExtensions lbi))
        ++ concat [opts | (GHC,opts) <- options lbi]
        ++ map ("-optP" ++) (cppOptions lbi)
        ++ modules
    let pkgDb = buildDir </> "db"
    command_ [] "ghc-pkg" ["init", pkgDb]
    let specPath = buildDir </> "spec"
    writeFile' specPath $ unlines
        [ "name: " ++ display (packageName (package desc))
        , "version: " ++ display (packageVersion (package desc))
        , "id: " ++ display (package desc)
        , "key: " ++ display (package desc)
        , "exposed-modules: " ++ unwords (map display $ exposedModules lib)
        , "hidden-modules: " ++ unwords (map display $ otherModules lbi)
        , "import-dirs: ${pkgroot}/hi"
        ]
    command_ [] "ghc-pkg" ["--package-db", pkgDb, "register", specPath]
    let res = BuiltPackage
                { builtTransitiveDBs =
                    HS.insert (buildDir </> "db")
                        $ foldMap builtTransitiveDBs deps
                , builtPackageName = packageName $ package desc
                }
    return res

pathsModule :: PackageName -> ModuleName
pathsModule p = fromString $ "Paths_" ++ map fixHyphen (display p)
  where
    fixHyphen '-' = '_'
    fixHyphen c = c

buildPlanDir :: PlanName -> FilePath
buildPlanDir (PlanName n) = buildArtifact $ n </> "packages"

packageBuildDir :: PlanName -> PackageId -> FilePath
packageBuildDir plan pkg = buildArtifact $ renderPlanName plan
                                    </> "packages"
                                    </> display pkg

packageDbFile :: FilePath
packageDbFile = "db/pkg.db"

-- TODO: this won't work when we need to do preprocessing.
moduleStr :: ModuleName -> String
moduleStr = intercalate "." . components

findModule :: FilePath -> PackageName -> [FilePath] -> ModuleName -> Action FilePath
findModule buildDir pkgName paths m
    | m == pathsModule pkgName = do
        let f = buildDir </> "paths" </> display m <.> "hs"
        need [f]
        return f
    | otherwise  = loop paths
  where
    loop [] = error $ "Missing module " ++ display m
                        ++ "; searched " ++ show paths
    loop (f:fs) = do
        let g = f </> toFilePath m <.> "hs"
        exists <- doesFileExist g
        if exists
            then return g
            else loop fs

pathsModuleRule :: Rules ()
pathsModuleRule =
    "build/*/packages/*/paths/*.hs" #> \f [lts,pkg',modName] -> do
        plan <- askBuildPlan $ PlanName lts
        Just pkg <- return $ simpleParse pkg'
        createParentIfMissing f
        writeFile' f $ unlines
            [ "{-# LANGUAGE CPP #-}"
            , "module " ++ modName ++ " (version) where"
            , "import Data.Version (Version(..))"
            , "version :: Version"
            , "version = Version " ++ show (versionBranch
                                                $ pkgVersion pkg)
                                    ++ ""
                            ++ " []" -- tags are deprecated
            ]
