{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Development.Stake.Build
    ( buildPackageRules
    , askBuiltPackages
    )
    where

import Control.Applicative (liftA2, (<|>))
import Control.Monad (filterM, guard, msum, void, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.Bifunctor (first)
import Data.List (intercalate, nub)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Traversable (for)
import GHC.Generics hiding (packageName)
import qualified Data.HashMap.Strict as HM
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Stake.Command
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
import Distribution.Simple.Utils (matchFileGlob)
import Distribution.System (buildOS, buildArch, OS(..))
import Distribution.Verbosity (normal)
import Distribution.Version (withinRange, Version(..))
import qualified Data.Set as Set
import Data.Set (Set)
import Language.Haskell.Extension
import qualified System.Directory as Directory

instance Hashable FlagName

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
                        , builtTransitiveIncludeDirs :: Set FilePath
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
        ParseFailed err -> error (show err)
        ParseOk _ info -> info
    return BuiltPackage { builtTransitiveDBs = Set.empty
                        , builtTransitiveLibFiles = Set.empty
                        , builtPackageName = packageName p
                        , builtTransitiveIncludeDirs
                            = Set.fromList $ IP.includeDirs info
                        }
buildResolved planName (Resolved Additional p) = do
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
buildLibrary planName plan deps desc lib = do
    let pkgPrefixDir = display (packageName $ package desc)
    let lbi = libBuildInfo lib
    let pkgDir = (packageSourceDir (package desc) </>)
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
                            (packageName $ package desc)
                            sourceDirs)
                        modules
    let libName = "HS" ++ display (packageName $ package desc)
    let libFile = pkgPrefixDir </> "lib" ++ libName ++ "-ghc"
                                ++ display (ghcVersion plan) <.> dynExt
    cInputs <- liftIO $ collectCFiles desc lbi pkgDir
    -- TODO: Actual LTS version ghc.
    let shouldBuildLib = not $ null $ exposedModules lib
    let compileOut = liftA2 (\lib hi -> (lib, Set.insert lib $ Set.fromList hi))
                        (output libFile)
                        (for modules
                            $ \m -> output $ hiDir </> toFilePath m <.> "dyn_hi")
    (maybeLib, libFiles)  <- if not shouldBuildLib
            then return (Nothing, Set.empty)
            else fmap (first Just)
                    . runCommand
                        compileOut
                        (Set.fromList moduleFiles
                            <> foldMap builtTransitiveDBs deps
                            <> foldMap builtTransitiveLibFiles deps
                            <> Set.fromList (map UserFile cInputs))
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
        concat (map (\p -> ["-package-db", takeDirectory $ relPath p])
                $ Set.toList
                $ foldMap builtTransitiveDBs deps)
        ++
        concat [["-package", display $ builtPackageName d]
                | d <- deps]
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
        ++ map relPath moduleFiles
        ++ map pkgDir (cSources lbi)
        ++ ["-optc" ++ opt | opt <- ccOptions lbi]
        ++ ["-l" ++ lib | lib <- extraLibs lbi]
        -- TODO: linker options too?
    spec <- writeArtifact (pkgPrefixDir </> "spec") $ unlines $
        [ "name: " ++ display (packageName (package desc))
        , "version: " ++ display (packageVersion (package desc))
        , "id: " ++ display (package desc)
        , "key: " ++ display (package desc)
        , "extra-libraries: " ++ unwords (extraLibs lbi)
        ]
        ++ case maybeLib of
            Nothing -> []
            Just libFile ->
                     [ "hs-libraries: " ++ libName
                     , "library-dirs: ${pkgroot}"
                     , "import-dirs: ${pkgroot}/hi"
                     , "exposed-modules: " ++ unwords (map display $ exposedModules lib)
                     , "hidden-modules: " ++ unwords (map display $ otherModules lbi)
                     ]
    let conf = display (packageName $ package desc) ++ "-" ++
                display (packageVersion $ package desc)
                <.> "conf"
    pkgDb' <-
        let relPkgDb = pkgPrefixDir </> "db"
        in runCommand (output $ relPkgDb </> "package.cache")
                (Set.singleton spec <> libFiles)
                $ prog "rmdir" [relPkgDb] -- ghc-pkg complains if the directory already
                                          -- exists
                    <> prog "ghc-pkg" ["init", relPkgDb]
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
    -> PackageDescription
    -> BuildInfo
    -> Set FilePath -- ^ Transitive C include dirs
    -> (FilePath -> FilePath) -- ^ Function to resolve relative source paths
                              -- within this package
    -> PackageName
    -> [FilePath]             -- Source directory to check
    -> ModuleName
    -> Action Artifact
findModule plan desc bi cIncludeDirs pkgDir pkgName paths m = do
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
    -> Set FilePath -- ^ Transitive C include dirs
    -> ModuleName
    -> (FilePath -> FilePath) -- ^ Resolve relative source paths
    -> FilePath -- ^ Source directory to check
    -> MaybeT Action Artifact
search plan desc bi cIncludeDirs m pkgDir srcDir
    = genHsc2hs <|> genHappy "l" <|> genHappy "ly"
                    <|> existing
  where
    existing = let f = srcDir </> toFilePath m <.> "hs"
                 in exists f >> return (UserFile f)
    genHappy ext = do
        let yFile = srcDir </> toFilePath m <.> ext
        exists yFile
        let relOutput = toFilePath m <.> "hs"
        lift
            . runCommand (output relOutput)
                (Set.singleton $ UserFile yFile)
                $ prog "happy"
                     ["-o", relOutput, yFile]
    genHsc2hs = do
        let hsc = srcDir </> toFilePath m <.> "hsc"
        exists hsc
        let relOutput = toFilePath m <.> "hs"
        cInputs <- liftIO $ collectCFiles desc bi pkgDir
        lift $ runCommand (output relOutput)
                (Set.fromList (map UserFile $ hsc : cInputs))
                $ prog "hsc2hs" $
                      ["-o", relOutput
                      , hsc
                      ]
                      -- TODO: CPP options?
                      ++ ["--cflag=" ++ f | f <- ccOptions bi]
                      ++ ["-I" ++ pkgDir f | f <- "" : includeDirs bi
                                               ++ Set.toList cIncludeDirs
                                               ]
                      ++ ["-D__GLASGOW_HASKELL__="
                            ++ cppVersion (ghcVersion plan)]

collectCFiles :: PackageDescription -> BuildInfo -> (FilePath -> FilePath) -> IO [FilePath]
collectCFiles desc bi pkgDir = do
    includeInputs <- findIncludeInputs pkgDir bi
    extras <- fmap concat $ mapM matchFileGlob
                          $ map pkgDir $ extraSrcFiles desc
    return $ includeInputs ++ map pkgDir (cSources bi) ++ extras

findIncludeInputs :: (FilePath -> FilePath) -> BuildInfo -> IO [FilePath]
findIncludeInputs pkgDir bi = filterM Directory.doesFileExist candidates
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

exists :: MonadIO m => FilePath -> MaybeT m ()
exists f = liftIO (Directory.doesFileExist f) >>= guard
