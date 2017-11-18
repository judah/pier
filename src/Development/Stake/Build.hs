{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Development.Stake.Build
    ( buildPackageRules
    , askBuiltPackages
    )
    where

import Control.Applicative (liftA2, (<|>))
import Control.Monad (filterM, guard, msum)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.Bifunctor (first)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Semigroup
import GHC.Generics hiding (packageName)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Stake.Command
import Development.Stake.Core
import Development.Stake.Package
import Development.Stake.Stackage
import Development.Stake.Witness
import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import qualified Distribution.InstalledPackageInfo as IP
import Distribution.Text
import Distribution.System (buildOS, OS(..))
import Distribution.Version (Version(..))
import Distribution.Compiler
import qualified Data.Set as Set
import Data.Set (Set)
import Language.Haskell.Extension


buildPackageRules :: Rules ()
buildPackageRules = do
    addWitness buildPackage


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
                        { builtTransitiveDBs :: Set Artifact
                        , builtTransitiveLibFiles :: Set Artifact
                        , builtPackageName :: PackageName
                        , builtTransitiveIncludeDirs :: Set Artifact
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
    result <- runCommandStdout
                Set.empty
                $ prog "ghc-pkg"
                    [ "describe"
                    , "--no-user-package-db"
                    , "--no-user-package-conf"
                    , display p
                    ]
    info <- return $! case IP.parseInstalledPackageInfo result of
        IP.ParseFailed err -> error (show err)
        IP.ParseOk _ info -> info
    return BuiltPackage { builtTransitiveDBs = Set.empty
                        , builtTransitiveLibFiles = Set.empty
                        , builtPackageName = packageName p
                        , builtTransitiveIncludeDirs
                            = Set.fromList $ map externalFile $ IP.includeDirs info
                        }
buildResolved planName (Resolved Additional p) = do
    plan <- askBuildPlan planName
    (desc, packageSourceDir) <- unpackedCabalPackageDir plan p
    buildFromDesc planName plan packageSourceDir desc

buildFromDesc :: PlanName -> BuildPlan -> Artifact -> PackageDescription -> Action BuiltPackage
buildFromDesc planName plan packageSourceDir desc
    | Just lib <- library desc
    , let lbi = libBuildInfo lib
    , buildable lbi = do
            let deps = [n | Dependency n _ <- targetBuildDepends
                                                lbi]
            builtDeps <- askBuiltPackages planName deps
            putNormal $ "Building " ++ display (package desc)
            buildLibrary plan builtDeps packageSourceDir desc lib
    | otherwise = error "buildFromDesc: no library"

buildLibrary
    :: BuildPlan -> [BuiltPackage]
    -> Artifact
    -> PackageDescription -> Library
    -> Action BuiltPackage
buildLibrary plan deps packageSourceDir desc lib = do
    let pkgPrefixDir = display (packageName $ package desc)
    let lbi = libBuildInfo lib
    let pkgDir = (packageSourceDir />)
    let sourceDirs = (\ss -> if null ss then [pkgDir "."] else ss)
                        $ map pkgDir
                        $ hsSourceDirs lbi
    let hiDir = pkgPrefixDir </> "hi"
    let oDir = pkgPrefixDir </> "o"
    let transitiveIncludeDirs = foldMap builtTransitiveIncludeDirs deps
    let modules = otherModules lbi ++ exposedModules lib
    moduleFiles <- mapM (findModule plan desc lbi
                            transitiveIncludeDirs
                            pkgDir
                            sourceDirs)
                        modules
    let libName = "HS" ++ display (packageName $ package desc)
    let libFile = pkgPrefixDir </> "lib" ++ libName ++ "-ghc"
                                ++ display (ghcVersion plan) <.> dynExt
    cInputs <- collectCFiles desc lbi pkgDir
    -- TODO: Actual LTS version ghc.
    let shouldBuildLib = not $ null $ exposedModules lib
    let compileOut = liftA2 (\linked hi -> (linked, Set.fromList [linked,hi]))
                        (output libFile)
                        (output hiDir)
    (maybeLinked, libFiles)  <- if not shouldBuildLib
            then return (Nothing, Set.empty)
            else fmap (first Just)
                    . runCommand
                        compileOut
                        (Set.fromList moduleFiles
                            <> foldMap builtTransitiveDBs deps
                            <> foldMap builtTransitiveLibFiles deps
                            <> Set.fromList cInputs)
                        $ prog "ghc" $
        [ "-ddump-to-file"
        , "-this-unit-id", display $ package desc
        -- Clear any existing package DB, including the `GHC_PACKAGE_PATH`
        -- which in particular is set by `stack` and `stack ghci`.
        , "-clear-package-db"
        , "-global-package-db"
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
        , "-o", libFile
        ]
        ++
        concat (map (\p -> ["-package-db", relPath p])
                $ Set.toList
                $ foldMap builtTransitiveDBs deps)
        ++
        concat [["-package", display $ builtPackageName d]
                | d <- deps]
        ++ map ("-I"++) (map (relPath . pkgDir) $ includeDirs lbi)
        ++ map ("-X" ++)
            (display (fromMaybe Haskell98 $ defaultLanguage lbi)
            : map display (defaultExtensions lbi ++ oldExtensions lbi))
        ++ concat [opts | (GHC,opts) <- options lbi]
        ++ map ("-optP" ++) (cppOptions lbi)
        -- TODO: configurable
        ++ ["-O0"]
        -- TODO: enable warnings for local builds
        ++ ["-w"]
        ++ map relPath moduleFiles
        ++ map (relPath . pkgDir) (cSources lbi)
        ++ ["-optc" ++ opt | opt <- ccOptions lbi]
        ++ ["-l" ++ libDep | libDep <- extraLibs lbi]
        -- TODO: linker options too?
    spec <- writeArtifact (pkgPrefixDir </> "spec") $ unlines $
        [ "name: " ++ display (packageName (package desc))
        , "version: " ++ display (packageVersion (package desc))
        , "id: " ++ display (package desc)
        , "key: " ++ display (package desc)
        , "extra-libraries: " ++ unwords (extraLibs lbi)
        ]
        ++ case maybeLinked of
            Nothing -> []
            Just _ ->
                     [ "hs-libraries: " ++ libName
                     , "library-dirs: ${pkgroot}"
                     , "import-dirs: ${pkgroot}/hi"
                     , "exposed-modules: " ++ unwords (map display $ exposedModules lib)
                     , "hidden-modules: " ++ unwords (map display $ otherModules lbi)
                     ]
    pkgDb' <-
        let relPkgDb = pkgPrefixDir </> "db"
        in runCommand (output relPkgDb)
                (Set.singleton spec <> libFiles)
                $ prog "ghc-pkg" ["init", relPkgDb]
                    <> prog "ghc-pkg" ["-v0", "--package-db", relPkgDb, "register",
                                   relPath spec]
    return BuiltPackage
        { builtTransitiveDBs =
            Set.insert pkgDb' $ foldMap builtTransitiveDBs deps
        , builtPackageName = packageName $ package desc
        , builtTransitiveLibFiles = libFiles
                <> foldMap builtTransitiveLibFiles deps
        -- TODO:
        , builtTransitiveIncludeDirs = Set.empty
        }

dynExt :: String
dynExt = case buildOS of
        OSX -> "dylib"
        _ -> "so"

-- TODO: Organize the arguments to this function better.
findModule
    :: BuildPlan
    -> PackageDescription
    -> BuildInfo
    -> Set Artifact -- ^ Transitive C include dirs
    -> (FilePath -> Artifact) -- ^ Function to resolve relative source paths
                              -- within this package
    -> [Artifact]             -- Source directory to check
    -> ModuleName
    -> Action Artifact
findModule plan desc bi cIncludeDirs pkgDir paths m = do
    found <- runMaybeT $ genPathsModule m (package desc) <|>
                msum (map (search plan desc bi cIncludeDirs m pkgDir) paths)
    case found of
        Nothing -> error $ "Missing module " ++ display m
                        ++ "; searched " ++ show paths
        Just f -> return f

genPathsModule
    :: ModuleName -> PackageIdentifier -> MaybeT Action Artifact
genPathsModule m pkg = do
    guard $ m == pathsModule
    lift $ writeArtifact ("paths" </> display m <.> "hs") $ unlines
       [ "{-# LANGUAGE CPP #-}"
        , "module " ++ display m ++ " (getDataFileName, getDataDir, version) where"
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
  where
    pathsModule = fromString $ "Paths_" ++ map fixHyphen (display $ pkgName pkg)
    fixHyphen '-' = '_'
    fixHyphen c = c


search
    :: BuildPlan
    -> PackageDescription
    -> BuildInfo
    -> Set Artifact -- ^ Transitive C include dirs
    -> ModuleName
    -> (FilePath -> Artifact) -- ^ Resolve relative source paths
    -> Artifact -- ^ Source directory to check
    -> MaybeT Action Artifact
search plan desc bi cIncludeDirs m pkgDir srcDir
    = genHsc2hs <|> genHappy "l" <|> genHappy "ly"
                    <|> existing
  where
    existing = let f = srcDir /> (toFilePath m <.> "hs")
                 in exists f >> return f
    genHappy ext = do
        let yFile = srcDir /> (toFilePath m <.> ext)
        exists yFile
        let relOutput = toFilePath m <.> "hs"
        lift
            . runCommand (output relOutput)
                (Set.singleton yFile)
                $ prog "happy"
                     ["-o", relOutput, relPath yFile]
    genHsc2hs = do
        let hsc = srcDir /> (toFilePath m <.> "hsc")
        exists hsc
        let relOutput = toFilePath m <.> "hs"
        cInputs <- lift $ collectCFiles desc bi pkgDir
        lift $ runCommand (output relOutput)
                (Set.fromList $ hsc : cInputs)
                $ prog "hsc2hs" $
                      ["-o", relOutput
                      , relPath hsc
                      ]
                      -- TODO: CPP options?
                      ++ ["--cflag=" ++ f | f <- ccOptions bi]
                      ++ ["-I" ++ relPath f | f <- fmap pkgDir ("" : includeDirs bi)
                                               ++ Set.toList cIncludeDirs
                                               ]
                      ++ ["-D__GLASGOW_HASKELL__="
                            ++ cppVersion (ghcVersion plan)]

collectCFiles :: PackageDescription -> BuildInfo -> (FilePath -> Artifact) -> Action [Artifact]
collectCFiles desc bi pkgDir = do
    includeInputs <- findIncludeInputs pkgDir bi
    extras <- fmap concat $ mapM (\f -> matchArtifactGlob (pkgDir "") f)
                            $ extraSrcFiles desc
    return $ includeInputs ++ map pkgDir (cSources bi) ++ extras

findIncludeInputs :: (FilePath -> Artifact) -> BuildInfo -> Action [Artifact]
findIncludeInputs pkgDir bi = filterM doesArtifactExist candidates
  where
    candidates = nub -- TODO: more efficient
                 [ pkgDir $ d </> f
                -- TODO: maybe just installIncludes shouldn't be prefixed
                -- with include dir?
                 | d <- "." : includeDirs bi
                 , f <- includes bi ++ installIncludes bi
                 ]

cppVersion :: Version -> String
cppVersion v = case versionBranch v of
    (v1:v2:_) -> show v1 ++ if v2 < 10 then '0':show v2 else show v2
    _ -> error $ "cppVersion: " ++ display v

exists :: Artifact -> MaybeT Action ()
exists f = lift (doesArtifactExist f) >>= guard
