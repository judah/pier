{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Development.Stake.Build
    ( buildPackageRules
    , askBuiltLibrary
    , askMaybeBuiltLibrary
    , buildExecutables
    , buildExecutableNamed
    , BuiltExecutable(..)
    )
    where

import Control.Applicative (liftA2, (<|>))
import Control.Monad (filterM, guard, msum)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.List (find, intercalate, nub)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup
import GHC.Generics hiding (packageName)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath hiding (exe)
import Development.Stake.Build.Custom
import Development.Stake.Command
import Development.Stake.Config
import Development.Stake.Package
import Development.Stake.Stackage
import Development.Stake.Persistent
import Distribution.ModuleName
import Distribution.Simple.Build.Macros (generatePackageVersionMacros)
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
buildPackageRules = addPersistent buildLibrary

newtype BuiltLibraryR = BuiltLibraryR PackageName
    deriving (Show,Typeable,Eq,Generic)
instance Hashable BuiltLibraryR
instance Binary BuiltLibraryR
instance NFData BuiltLibraryR
type instance RuleResult BuiltLibraryR = Maybe BuiltLibrary

data TransitiveDeps = TransitiveDeps
    { transitiveDBs :: Set Artifact
    , transitiveLibFiles :: Set Artifact
    , transitiveIncludeDirs :: Set Artifact
    , transitiveDataFiles :: Set Artifact
    } deriving (Show, Eq, Typeable, Generic, Hashable, Binary, NFData)

instance Semigroup TransitiveDeps

instance Monoid TransitiveDeps where
    mempty = TransitiveDeps Set.empty Set.empty Set.empty Set.empty
    TransitiveDeps dbs files is datas
        `mappend` TransitiveDeps dbs' files' is' datas'
        = TransitiveDeps (dbs <> dbs') (files <> files') (is <> is')
                (datas <> datas')


-- ghc --package-db .stake/...text-1234.pkg/db --package text-1234
data BuiltLibrary = BuiltLibrary
    { builtPackageId :: PackageIdentifier
    , builtPackageTrans :: TransitiveDeps
    }
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)

askBuiltLibraries :: [PackageName] -> Action [BuiltLibrary]
askBuiltLibraries = flip forP askBuiltLibrary

askMaybeBuiltLibrary :: PackageName -> Action (Maybe BuiltLibrary)
askMaybeBuiltLibrary pkg = askPersistent (BuiltLibraryR pkg)

askBuiltLibrary :: PackageName -> Action BuiltLibrary
askBuiltLibrary pkg = askMaybeBuiltLibrary pkg >>= helper
  where
    helper Nothing = error $ "buildFromDesc: " ++ display pkg
                                ++ " does not have a buildable library"
    helper (Just lib) = return lib


data BuiltDeps = BuiltDeps [PackageIdentifier] TransitiveDeps

askBuiltDeps
    :: [PackageName]
    -> Action BuiltDeps
askBuiltDeps pkgs = do
    deps <- askBuiltLibraries pkgs
    return $ BuiltDeps (dedup $ map builtPackageId deps)
                  (foldMap builtPackageTrans deps)
  where
    dedup = Set.toList . Set.fromList

-- TODO: merge with Resolved
-- TODO: don't copy everything if configuring a local package?  Or at least
-- treat deps less coarsely?
getConfiguredPackage
    :: PackageName -> Action (Either PackageId ConfiguredPkg)
getConfiguredPackage p = do
    conf <- askConfig
    case resolvePackage conf p of
        Builtin pid -> return $ Left pid
        Hackage pid -> do
            dir <- getPackageSourceDir pid
            Right <$> getConfigured conf dir
        Local dir _ -> Right <$> getConfigured conf dir
  where
    getConfigured :: Config -> Artifact -> Action ConfiguredPkg
    getConfigured conf dir = do
        (desc, dir') <- configurePackage (plan conf) dir
        macros <- genCabalMacros conf desc
        return $ ConfiguredPkg desc dir' macros


buildLibrary :: BuiltLibraryR -> Action (Maybe BuiltLibrary)
buildLibrary (BuiltLibraryR pkg) =
    getConfiguredPackage pkg >>= \case
        Left p -> Just <$> getBuiltinLib p
        Right confd
            | Just lib <- library (confdDesc confd)
            , let bi = libBuildInfo lib
            , buildable bi -> Just <$> do
                deps <- askBuiltDeps $ targetDepNames bi
                buildLibraryFromDesc deps confd lib
            | otherwise -> return Nothing

getBuiltinLib :: PackageId -> Action BuiltLibrary
getBuiltinLib p = do
    ghc <- configGhc <$> askConfig
    result <- runCommandStdout
                $ ghcPkgProg ghc
                    ["describe" , display p]

    info <- return $! case IP.parseInstalledPackageInfo result of
        IP.ParseFailed err -> error (show err)
        IP.ParseOk _ info -> info
    return $ BuiltLibrary p
                TransitiveDeps
                    { transitiveDBs = Set.empty
                    , transitiveLibFiles = ghcArtifacts ghc
                    , transitiveIncludeDirs
                            = Set.fromList
                                    $ map (parseGlobalPackagePath ghc)
                                    $ IP.includeDirs info
                    , transitiveDataFiles = Set.empty
                    }

buildLibraryFromDesc
    :: BuiltDeps
    -> ConfiguredPkg
    -> Library
    -> Action BuiltLibrary
buildLibraryFromDesc deps@(BuiltDeps _ transDeps) confd lib = do
    let packageSourceDir = confdSourceDir confd
    let desc = confdDesc confd
    conf <- askConfig
    let ghc = configGhc conf
    let lbi = libBuildInfo lib
    let hiDir = "hi"
    let oDir = "o"
    let libName = "HS" ++ display (packageName $ package desc)
    let libFile = "lib" ++ libName <.> "a"
    let dynLibFile = "lib" ++ libName
                        ++ "-ghc" ++ display (ghcVersion $ plan conf) <.> dynExt
    let shouldBuildLib = not $ null $ exposedModules lib
    let pkgDir = (packageSourceDir />)
    let modules = otherModules lbi ++ exposedModules lib
    let cIncludeDirs = transitiveIncludeDirs transDeps
                        <> Set.map pkgDir (Set.fromList $ ifNullDirs
                                                $ includeDirs lbi)
    let cFiles = map pkgDir $ cSources lbi
    datas <- collectDataFiles ghc desc packageSourceDir
    moduleFiles <- mapM (findModule ghc desc lbi cIncludeDirs datas
                            $ sourceDirArtifacts packageSourceDir lbi)
                        modules
    moduleBootFiles <- catMaybes <$> mapM findBootFile moduleFiles
    cIncludes <- collectCIncludes desc lbi pkgDir
    maybeLib <- if not shouldBuildLib
            then return Nothing
            else do
                (hiDir', oDir') <- runCommand
                    (liftA2 (,) (output hiDir) (output oDir))
                    $ message (display (package desc) ++ ": building library")
                    <> inputList (moduleBootFiles ++ cIncludes)
                    <> ghcCommand ghc deps lbi confd
                            [ "-this-unit-id", display $ package desc
                            , "-hidir", pathOut hiDir
                            , "-odir", pathOut oDir
                            , "-dynamic-too"
                            ]
                            (moduleFiles ++ cFiles)
                let objs = map (\m -> oDir' /> toFilePath m <.> "o") modules
                                -- TODO: this is pretty janky...
                                ++ map (\f -> replaceArtifactExtension
                                                    (oDir'/> pathIn f) "o")
                                        cFiles
                let dynModuleObjs = map (\m -> oDir' /> toFilePath m <.> "dyn_o") modules
                libArchive <- runCommand (output libFile)
                    $ inputList objs
                    <> message (display (package desc) ++ ": linking static library")
                    <> prog "ar" ([arParams, pathOut libFile]
                                    ++ map pathIn objs)
                dynLib <- runCommand (output dynLibFile)
                    $ inputList cIncludes
                    <> message (display (package desc) ++ ": linking dynamic library")
                    <> ghcCommand ghc deps lbi confd
                        ["-shared", "-dynamic", "-o", pathOut dynLibFile]
                        (dynModuleObjs ++ cFiles)
                return $ Just (libName, lib, libArchive, dynLib, hiDir')
    (pkgDb, libFiles) <- registerPackage ghc (package desc) lbi maybeLib
                deps
    return $ BuiltLibrary (package desc)
            $ transDeps <> TransitiveDeps
                { transitiveDBs = Set.singleton pkgDb
                , transitiveLibFiles = Set.singleton libFiles
                -- TODO:
                , transitiveIncludeDirs = Set.empty
                , transitiveDataFiles = maybe Set.empty Set.singleton datas
                }

arParams :: String
arParams = case buildOS of
                OSX -> "-cqv"
                _ -> "-rcs"

-- TODO: double-check no two executables with the same name

data BuiltExecutable = BuiltExecutable
    { builtBinary :: Artifact
    , builtExeDataFiles :: Set Artifact
    }

progExe :: BuiltExecutable -> [String] -> Command
progExe exe args = progA (builtBinary exe) args
                <> inputs (builtExeDataFiles exe)

-- TODO: figure out the whole caching story
buildExecutables :: PackageName -> Action (Map.Map String BuiltExecutable)
buildExecutables p = getConfiguredPackage p >>= \case
    Left _ -> return Map.empty
    Right confd ->
        fmap Map.fromList
            . mapM (\e -> (exeName e,) <$> buildExecutable confd e)
            . filter (buildable . buildInfo)
            $ executables (confdDesc confd)

-- TODO: error if not buildable?
buildExecutableNamed :: PackageName -> String -> Action BuiltExecutable
buildExecutableNamed p e = getConfiguredPackage p >>= \case
    Left pid -> error $ "Built-in package " ++ display pid
                        ++ " has no executables"
    Right confd
        | Just exe <- find ((== e) . exeName) (executables $ confdDesc confd)
            -> buildExecutable confd exe
        | otherwise -> error $ "Package " ++ display (packageId confd)
                            ++ " has no executable named " ++ e

buildExecutable
    :: ConfiguredPkg
    -> Executable
    -> Action BuiltExecutable
buildExecutable confd exe = do
    let desc = confdDesc confd
    let packageSourceDir = confdSourceDir confd
    let bi = buildInfo exe
    deps@(BuiltDeps _ transDeps)
        <- askBuiltDeps $ targetDepNames bi
    conf <- askConfig
    let ghc = configGhc conf
    let outputPrefix = display (packageName $ package desc)
                        </> "exe" </> exeName exe
    let cIncludeDirs = transitiveIncludeDirs transDeps
                        <> Set.map (packageSourceDir />)
                                 (Set.fromList $ ifNullDirs $ includeDirs bi)
    datas <- collectDataFiles ghc desc packageSourceDir
    let findM = findModule ghc desc bi cIncludeDirs datas
                    $ sourceDirArtifacts packageSourceDir bi
    otherModuleFiles <- mapM findM $ addIfMissing pathsMod $ otherModules bi
    mainFile <- let fullPath = packageSourceDir /> modulePath exe
                in doesArtifactExist fullPath >>= \case
                            True -> return fullPath
                            False -> findM $ filePathToModule $ modulePath exe
    moduleBootFiles <- catMaybes <$> mapM findBootFile otherModuleFiles
    let cFiles = map (packageSourceDir />) $ cSources bi
    cIncludes <- collectCIncludes desc bi (packageSourceDir />)
    bin <- runCommand (output $ exeName exe)
        $ message (display (package desc) ++ ": building executable "
                    ++ exeName exe)
        <> inputList moduleBootFiles
        <> inputList cIncludes
        <> ghcCommand ghc deps bi confd
                [ "-o", pathOut (exeName exe)
                , "-hidir", outputPrefix </> "hi"
                , "-odir", outputPrefix </> "o"
                ]
                (addIfMissing mainFile otherModuleFiles ++ cFiles)
    return BuiltExecutable
        { builtBinary = bin
        , builtExeDataFiles = foldr Set.insert (transitiveDataFiles transDeps)
                                    datas
        }
  where
    pathsMod = fromString $ "Paths_" ++ display (packageName confd)
    addIfMissing m ms
        | m `elem` ms = ms
        | otherwise = m : ms


-- TODO: issue if this doesn't preserve ".lhs" vs ".hs", for example?
filePathToModule :: FilePath -> ModuleName
filePathToModule = fromString . intercalate "." . splitDirectories . dropExtension

ghcCommand
    :: InstalledGhc
    -> BuiltDeps
    -> BuildInfo
    -> ConfiguredPkg
    -> [String]
    -> [Artifact]
    -> Command
ghcCommand ghc (BuiltDeps depPkgs transDeps) bi confd
    extraArgs ghcInputs
        = ghcProg ghc (args ++ map pathIn ghcInputs)
            <> inputs (transitiveDBs transDeps)
            <> inputs (transitiveLibFiles transDeps)
            <> inputList ghcInputs
            <> input (confdMacros confd)
  where
    packageSourceDir = confdSourceDir confd
    pkgDir = (packageSourceDir />)
    extensions =
        display (fromMaybe Haskell98 $ defaultLanguage bi)
            : map display (defaultExtensions bi ++ oldExtensions bi)
    args =
        -- Rely on GHC for module ordering and hs-boot files:
        [ "--make"
        , "-v0"
        , "-fPIC"
        , "-i"
        ]
        -- Necessary for boot files:
        ++ map (("-i" ++) . pathIn) (sourceDirArtifacts packageSourceDir bi)
        ++
        concatMap (\p -> ["-package-db", pathIn p])
                (Set.toList $ transitiveDBs transDeps)
        ++
        concat [["-package", display d] | d <- depPkgs]
        ++ map (("-I" ++) . pathIn . pkgDir) (includeDirs bi)
        ++ map ("-X" ++) extensions
        ++ concat [opts | (GHC,opts) <- options bi]
        ++ map ("-optP" ++) (cppOptions bi)
        ++ ["-optP-include", "-optP" ++ pathIn (confdMacros confd)]
        -- TODO: configurable
        ++ ["-O0"]
        -- TODO: enable warnings for local builds
        ++ ["-w"]
        ++ ["-optc" ++ opt | opt <- ccOptions bi]
        ++ ["-l" ++ libDep | libDep <- extraLibs bi]
        -- TODO: linker options too?
        ++ extraArgs

sourceDirArtifacts :: Artifact -> BuildInfo -> [Artifact]
sourceDirArtifacts packageSourceDir bi
    = map (packageSourceDir />) $ ifNullDirs $ hsSourceDirs bi

registerPackage
    :: InstalledGhc
    -> PackageIdentifier
    -> BuildInfo
    -> Maybe ( String  -- Library name for linking
             , Library
             , Artifact -- lib archive
             , Artifact -- dyn lib archive
             , Artifact -- hi
             )
    -> BuiltDeps
    -> Action (Artifact, Artifact)
registerPackage ghc pkg bi maybeLib (BuiltDeps depPkgs transDeps)
    = do
    let pre = "files"
    let (collectLibInputs, libDesc) = case maybeLib of
            Nothing -> (createDirectoryA pre, [])
            Just (libName, lib, libA, dynLibA, hi) ->
                ( shadow libA (pre </> takeFileName (pathIn libA))
                    <> shadow dynLibA (pre </> takeFileName (pathIn dynLibA))
                    <> shadow hi (pre </> "hi")
                , [ "hs-libraries: " ++ libName
                  , "library-dirs: ${pkgroot}" </> pre
                  , "dynamic-library-dirs: ${pkgroot}" </> pre
                  , "import-dirs: ${pkgroot}" </> pre </> "hi"
                  , "exposed-modules: " ++ unwords (map display $ exposedModules lib)
                  , "hidden-modules: " ++ unwords (map display $ otherModules bi)
                  ]
                )
    spec <- writeArtifact "spec" $ unlines $
        [ "name: " ++ display (packageName pkg)
        , "version: " ++ display (packageVersion pkg)
        , "id: " ++ display pkg
        , "key: " ++ display pkg
        , "extra-libraries: " ++ unwords (extraLibs bi)
        , "depends: " ++ unwords (map display depPkgs)
        ]
        ++ libDesc
    let db = "db"
    runCommand (liftA2 (,) (output db) (output pre))
        $ collectLibInputs
            <> ghcPkgProg ghc ["init", pathOut db]
            <> ghcPkgProg ghc
                    (["-v0"]
                    ++ [ "--package-db=" ++ pathIn f
                       | f <-  Set.toList $ transitiveDBs transDeps
                       ]
                    ++ ["--package-db", pathOut db, "register",
                               pathIn spec])
            <> input spec
            <> inputs (transitiveDBs transDeps)


dynExt :: String
dynExt = case buildOS of
        OSX -> "dylib"
        _ -> "so"

-- TODO: Organize the arguments to this function better.
findModule
    :: InstalledGhc
    -> PackageDescription
    -> BuildInfo
    -> Set Artifact -- ^ Transitive C include dirs
    -> Maybe Artifact     -- ^ data dir
    -> [Artifact]   -- ^ Source directory to check
    -> ModuleName
    -> Action Artifact
findModule ghc desc bi cIncludeDirs datas paths m = do
    found <- runMaybeT $ genPathsModule m (package desc) datas <|>
                msum (map (search ghc bi cIncludeDirs m) paths)
    case found of
        Nothing -> error $ "Missing module " ++ display m
                        ++ "; searched " ++ show paths
        Just f -> return f

genPathsModule
    :: ModuleName -> PackageIdentifier -> Maybe Artifact -> MaybeT Action Artifact
genPathsModule m pkg datas = do
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
        , "getDataFileName :: FilePath -> IO FilePath"
        , "getDataFileName f = (\\d -> d ++ \"/\" ++ f) <$> getDataDir"
        , "getDataDir :: IO FilePath"
        , "getDataDir = " ++ maybe err (("return " ++) . show . pathIn) datas
        ]
  where
    pathsModule = fromString $ "Paths_" ++ map fixHyphen (display $ pkgName pkg)
    fixHyphen '-' = '_'
    fixHyphen c = c
    err = "error " ++ show ("Missing data files from package " ++ display pkg)


search
    :: InstalledGhc
    -> BuildInfo
    -> Set Artifact -- ^ Transitive C include dirs
    -> ModuleName
    -> Artifact -- ^ Source directory to check
    -> MaybeT Action Artifact
search ghc bi cIncludeDirs m srcDir
    = genHsc2hs <|>
      genHappy "y" <|>
      genHappy "ly" <|>
      genAlex "x" <|>
      existing "lhs" <|>
      existing "hs"
  where
    genHappy ext = do
        let yFile = srcDir /> toFilePath m <.> ext
        exists yFile
        let relOutput = toFilePath m <.> "hs"
        happy <- lift $ buildExecutableNamed (PackageName "happy") "happy"
        lift . runCommand (output relOutput)
             $ progExe happy
                     ["-o", pathOut relOutput, pathIn yFile]
                <> input yFile

    genHsc2hs = do
        let hsc = srcDir /> toFilePath m <.> "hsc"
        exists hsc
        let relOutput = toFilePath m <.> "hs"
        lift $ runCommand (output relOutput)
             $ hsc2hsProg ghc
                      (["-o", pathOut relOutput
                       , pathIn hsc
                       ]
                       -- TODO: CPP options?
                       ++ ["--cflag=" ++ f | f <- ccOptions bi]
                       ++ ["-I" ++ pathIn f | f <- Set.toList cIncludeDirs]
                       ++ ["-D__GLASGOW_HASKELL__="
                             ++ cppVersion (ghcInstalledVersion ghc)])
                <> input hsc <> inputs cIncludeDirs

    genAlex ext = do
        let xFile = srcDir /> toFilePath m <.> ext
        exists xFile
        let relOutput = toFilePath m <.> "hs"
        alex <- lift $ buildExecutableNamed (PackageName "alex") "alex"
        lift . runCommand (output relOutput)
            $ progExe alex
                     ["-o", pathOut relOutput, pathIn xFile]
               <> input xFile

    existing ext = let f = srcDir /> toFilePath m <.> ext
                 in exists f >> return f

ifNullDirs :: [FilePath] -> [FilePath]
ifNullDirs [] = [""]
ifNullDirs xs = xs

-- Find the "hs-boot" file corresponding to a "hs" file.
findBootFile :: Artifact -> Action (Maybe Artifact)
findBootFile hs = do
    let hsBoot = replaceArtifactExtension hs "hs-boot"
    bootExists <- doesArtifactExist hsBoot
    return $ guard bootExists >> return hsBoot

collectCIncludes :: PackageDescription -> BuildInfo -> (FilePath -> Artifact) -> Action [Artifact]
collectCIncludes desc bi pkgDir = do
    includeInputs <- findIncludeInputs pkgDir bi
    extras <- fmap concat $ mapM (matchArtifactGlob (pkgDir ""))
                            $ extraSrcFiles desc
    return $ includeInputs ++ extras

findIncludeInputs :: (FilePath -> Artifact) -> BuildInfo -> Action [Artifact]
findIncludeInputs pkgDir bi = filterM doesArtifactExist candidates
  where
    candidates = nub -- TODO: more efficient
                 [ pkgDir $ d </> f
                -- TODO: maybe just installIncludes shouldn't be prefixed
                -- with include dir?
                 | d <- "" : includeDirs bi
                 , f <- includes bi ++ installIncludes bi
                 ]

collectDataFiles
    :: InstalledGhc -> PackageDescription -> Artifact -> Action (Maybe Artifact)
collectDataFiles ghc desc dir = case display (packageName desc) of
    "happy" -> Just <$> collectHappyDataFiles ghc dir
    "alex" -> Just <$> collectAlexDataFiles ghc dir
    _ -> collectPlainDataFiles desc dir

-- TODO: should we filter out packages without data-files?
-- Or short-cut it somewhere else?
collectPlainDataFiles
    :: PackageDescription -> Artifact -> Action (Maybe Artifact)
collectPlainDataFiles desc dir = do
    let inDir = dir /> dataDir desc
    let out = "data-files"
    if null (dataFiles desc)
        then return Nothing
        else fmap Just
            . runCommand (output out)
            $ prog "mkdir" ["-p", pathOut out]
                <> foldMap (\f -> shadow (inDir /> f) (out </> f))
                    (dataFiles desc)

cppVersion :: Version -> String
cppVersion v = case versionBranch v of
    (v1:v2:_) -> show v1 ++ if v2 < 10 then '0':show v2 else show v2
    _ -> error $ "cppVersion: " ++ display v

exists :: Artifact -> MaybeT Action ()
exists f = lift (doesArtifactExist f) >>= guard

data ConfiguredPkg = ConfiguredPkg
    { confdDesc :: PackageDescription
    , confdSourceDir :: Artifact
    , confdMacros :: Artifact
        -- Provides Cabal macros like VERSION_*
    }

instance Package ConfiguredPkg where
    packageId = packageId . confdDesc

-- For compatibility with Cabal, we generate a single macros file for the
-- entire package, rather than separately for the library, executables, etc.
-- For example, `pandoc-1.19.2.1`'s `pandoc` executable references
-- `VERSION_texmath` in `pandoc.hs`, despite not directly depending on the
-- `texmath` package.
genCabalMacros :: Config -> PackageDescription -> Action Artifact
genCabalMacros conf desc = do
    let allBis = [libBuildInfo l | Just l <- [library desc]]
                    ++ map buildInfo (executables desc)
                    ++ map testBuildInfo (testSuites desc)
                    ++ map benchmarkBuildInfo (benchmarks desc)
    let packageNames = Set.toList . Set.fromList . concatMap targetDepNames
                            $ allBis
    writeArtifact "macros.h"
        . generatePackageVersionMacros
        . map (resolvedPackageId . resolvePackage conf)
        $ packageNames

targetDepNames :: BuildInfo -> [PackageName]
targetDepNames bi = [n | Dependency n _ <- targetBuildDepends bi]
