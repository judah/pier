{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Development.Stake.Build
    ( buildPackageRules
    , askBuiltPackages
    )
    where

import Control.Applicative ((<|>))
import Control.Monad (guard, msum, void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup
import GHC.Generics hiding (packageName)
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
import qualified Distribution.InstalledPackageInfo as IP
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.System (buildOS, buildArch, OS(..))
import Distribution.Verbosity (normal)
import Distribution.Version (withinRange, Version(..))
import qualified Data.HashSet as HS
import Language.Haskell.Extension
import qualified System.Directory as Directory

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
                        , builtTransitiveIncludeDirs :: HS.HashSet FilePath
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
    Stdout result <- quietly $ command [] "ghc-pkg"
                        [ "describe"
                        , "--no-user-package-db"
                        , "--no-user-package-conf"
                        , display p
                        ]
    info <- return $! case IP.parseInstalledPackageInfo result of
        ParseFailed err -> error (show err)
        ParseOk _ info -> info
    return BuiltPackage { builtTransitiveDBs = HS.empty
                        , builtPackageName = packageName p
                        , builtTransitiveIncludeDirs
                            = HS.fromList $ IP.includeDirs info
                        }
buildResolved planName (Resolved Additional p) = do
    -- TODO: what if the .cabal file has a different basename?
    -- Maybe make this a witness too.
    let f = packageSourceDir p </> unPackageName (pkgName p) <.> "cabal"
    need [f]
    gdesc <- liftIO $ readPackageDescription normal f
    plan <- askBuildPlan planName
    desc <- flattenToDefaultFlags plan gdesc
    -- TODO: more relocatable
    dir <- liftIO $ Directory.getCurrentDirectory
    desc' <- case buildType desc of
        -- TODO: more hermetic
        Just Configure -> do
            command_ [Cwd $ packageSourceDir p]
                (dir </> packageSourceDir p </> "configure")
                []
            let buildInfoFile = packageSourceDir p
                                    </> unPackageName (pkgName p)
                                    <.> "buildinfo"
            buildInfoExists <- liftIO $ Directory.doesFileExist buildInfoFile
            case buildInfoExists of
                False -> return desc
                True -> do
                    hookedBI
                        <- liftIO $ readHookedBuildInfo normal buildInfoFile
                    return $ updatePackageDescription hookedBI desc
        _ -> return desc -- best effort

    buildFromDesc planName plan desc'

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
                -- TODO
                , builtTransitiveIncludeDirs = HS.empty
                }

  | otherwise = quietly $ do
    let lbi = libBuildInfo lib
    let pkgDir = (packageSourceDir (package desc) </>)
    let sourceDirs = (\ss -> if null ss then [pkgDir "."] else ss)
                        $ map pkgDir
                        $ hsSourceDirs lbi
    let buildDir = packageBuildDir planName (package desc)
    liftIO $ removeFiles buildDir ["//*"]
    let hiDir = buildDir </> "hi"
    let oDir = buildDir </> "o"
    let libDir = buildDir </> "lib"
    liftIO $ Directory.createDirectoryIfMissing True libDir
    let transitiveIncludeDirs = foldMap builtTransitiveIncludeDirs deps
    modules <- mapM (findModule plan lbi
                        transitiveIncludeDirs
                        buildDir pkgDir
                        (packageName $ package desc)
                        sourceDirs)
                    $ otherModules lbi ++ exposedModules lib
    let libName = "HS" ++ display (packageName $ package desc)
                      ++ "-" ++ renderPlanName planName
    -- TODO: Actual LTS version ghc.
    -- TODO: dump output only if the command fails.
    command_ [] "ghc" $
        [ "-ddump-to-file"
        , "-this-unit-id", display $ package desc
        , "-hide-all-packages"
        , "-i"
        , "-hidir", hiDir
        , "-odir", oDir
        , "-v0"
        -- TODO: allow static linking
        , "-dynamic"
        , "-hisuf", "dyn_hi"
        , "-osuf", "dyn_o"
        , "-shared"
        , "-fPIC"
        , "-o", libDir </> "lib" ++ libName
                            ++ "-ghc" ++ display (ghcVersion plan) <.> dynExt
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
            (display (fromMaybe Haskell98 $ defaultLanguage lbi)
            : map display (defaultExtensions lbi ++ oldExtensions lbi))
        ++ concat [opts | (GHC,opts) <- options lbi]
        ++ map ("-optP" ++) (cppOptions lbi)
        -- TODO: configurable
        ++ ["-O0"]
        -- TODO: enable warnings for local builds
        ++ ["-w"]
        ++ modules
        -- TODO: linker and cpp options too?
        ++ ["-optc" ++ opt | opt <- ccOptions lbi]
        ++ map pkgDir (cSources lbi)
        ++ ["-l" ++ lib | lib <- extraLibs lbi]
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
        , "hs-libraries: " ++ libName
        , "library-dirs: ${pkgroot}/lib"
        , "extra-libraries: " ++ unwords (extraLibs lbi)
        ]
    command_ [] "ghc-pkg" ["-v0", "--package-db", pkgDb, "register", specPath]
    let res = BuiltPackage
                { builtTransitiveDBs =
                    HS.insert (buildDir </> "db")
                        $ foldMap builtTransitiveDBs deps
                , builtPackageName = packageName $ package desc
                -- TODO:
                , builtTransitiveIncludeDirs = HS.empty
                }
    return res

dynExt :: String
dynExt = case buildOS of
        OSX -> "dylib"
        _ -> "so"

buildPlanDir :: PlanName -> FilePath
buildPlanDir (PlanName n) = buildArtifact $ n </> "packages"

packageBuildDir :: PlanName -> PackageId -> FilePath
packageBuildDir plan pkg = buildArtifact $ renderPlanName plan
                                    </> "packages"
                                    </> display pkg

packageDbFile :: FilePath
packageDbFile = "db/pkg.db"

moduleStr :: ModuleName -> String
moduleStr = intercalate "." . components

-- TODO: Organize the arguments to this function better.
findModule
    :: BuildPlan
    -> BuildInfo
    -> HS.HashSet FilePath -- ^ Transitive C include dirs
    -> FilePath -- ^ Build directory for outputs of this rule
    -> (FilePath -> FilePath) -- ^ Function to resolve relative source paths
                              -- within this package
    -> PackageName
    -> [FilePath]             -- Source directory to check
    -> ModuleName
    -> Action FilePath
findModule plan bi cIncludeDirs buildDir pkgDir pkgName paths m = do
    found <- runMaybeT $ genPathsModule buildDir m pkgName <|>
                msum (map (search plan bi cIncludeDirs m buildDir pkgDir) paths)
    case found of
        Nothing -> error $ "Missing module " ++ display m
                        ++ "; searched " ++ show paths
        Just f -> return f

genPathsModule
    :: FilePath -> ModuleName -> PackageName -> MaybeT Action FilePath
genPathsModule buildDir m pkg = do
    guard $ m == pathsModule
    let f = buildDir </> "paths" </> display m <.> "hs"
    lift $ need [f]
    return f
  where
    pathsModule = fromString $ "Paths_" ++ map fixHyphen (display pkg)
    fixHyphen '-' = '_'
    fixHyphen c = c


search
    :: BuildPlan
    -> BuildInfo
    -> HS.HashSet FilePath -- ^ Transitive C include dirs
    -> ModuleName
    -> FilePath -- ^ Build directory for outputs
    -> (FilePath -> FilePath) -- ^ Resolve relative source paths
    -> FilePath -- ^ Source directory to check
    -> MaybeT Action FilePath
search plan bi cIncludeDirs m buildDir pkgDir srcDir
    = genHsc2hs <|> genHappy "l" <|> genHappy "ly"
                    <|> existing
  where
    existing = let f = srcDir </> toFilePath m <.> "hs"
                 in exists f >> return f
    genHappy ext = do
        let input = srcDir </> toFilePath m <.> ext
        let output = buildDir </> "happy" </> toFilePath m <.> "hs"
        exists input
        createParentIfMissing output
        lift $ command_ [] "happy"
             $ ["-o", output, input]
        return output
    genHsc2hs = do
        let input = srcDir </> toFilePath m <.> "hsc"
        let output = buildDir </> "hsc2hs" </> toFilePath m <.> "hs"
        exists input
        createParentIfMissing output
        lift $ command_ [] "hsc2hs"
             $ ["-o", output
               , input
               ]
               -- TODO: CPP options?
               ++ ["--cflag=" ++ f | f <- ccOptions bi]
               ++ ["-I" ++ pkgDir f | f <- "" : includeDirs bi
                                        ++ HS.toList cIncludeDirs
                                        ]
               ++ ["-D__GLASGOW_HASKELL__=" ++ cppVersion (ghcVersion plan)]
        return output


cppVersion :: Version -> String
cppVersion v = case versionBranch v of
    (v1:v2:_) -> show v1 ++ if v2 < 10 then '0':show v2 else show v2
    _ -> error $ "cppVersion: " ++ display v

exists :: MonadIO m => FilePath -> MaybeT m ()
exists f = liftIO (Directory.doesFileExist f) >>= guard


-- TODO: more build-y

pathsModuleRule :: Rules ()
pathsModuleRule =
    "build/*/packages/*/paths/*.hs" #> \f [lts,pkg',modName] -> quietly $ do
        plan <- askBuildPlan $ PlanName lts
        Just pkg <- return $ simpleParse pkg'
        createParentIfMissing f
        writeFile' f $ unlines
            [ "{-# LANGUAGE CPP #-}"
            , "module " ++ modName ++ " (getDataFileName, getDataDir, version) where"
            , "import Data.Version (Version(..))"
            , "version :: Version"
            , "version = Version " ++ show (versionBranch
                                                $ pkgVersion pkg)
                                    ++ ""
                            ++ " []" -- tags are deprecated
            -- TODO:
            , "getDataFileName :: FilePath -> IO FilePath"
            , "getDataFileName = error \"getDataFileName: TODO\""
            , "getDataDir :: IO FilePath"
            , "getDataDir = error \"getDataDir: TODO\""
            ]
