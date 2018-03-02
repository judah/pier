{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Pier.Build.Components
    ( buildPackageRules
    , askBuiltLibrary
    , askMaybeBuiltLibrary
    , askBuiltExecutables
    , askBuiltExecutable
    , BuiltExecutable(..)
    )
    where

import Control.Applicative (liftA2, (<|>))
import Control.Monad (filterM, guard, msum)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.List (find, intercalate, nub)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup
import Data.Set (Set)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath hiding (exe)
import Distribution.Compiler
import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Build.Macros (generatePackageVersionMacros)
import Distribution.System (buildOS, OS(..))
import Distribution.Text
import Distribution.Version (Version, mkVersion, versionNumbers)
import GHC.Generics hiding (packageName)
import Language.Haskell.Extension

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Distribution.InstalledPackageInfo as IP

import Pier.Build.Custom
import Pier.Build.Config
import Pier.Build.Package
import Pier.Build.Stackage
import Pier.Core.Artifact
import Pier.Core.Persistent


buildPackageRules :: Rules ()
buildPackageRules = do
    addPersistent buildLibrary
    addPersistent getBuiltinLib
    addPersistent buildExecutables
    addPersistent buildExecutable

newtype BuiltLibraryQ = BuiltLibraryQ PackageName
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)
type instance RuleResult BuiltLibraryQ = Maybe BuiltLibrary

instance Show BuiltLibraryQ where
    show (BuiltLibraryQ p) = "Library " ++ display p

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


-- ghc --package-db .../text-1234.pkg/db --package text-1234
data BuiltLibrary = BuiltLibrary
    { builtPackageId :: PackageIdentifier
    , builtPackageTrans :: TransitiveDeps
    }
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)

askBuiltLibraries :: [PackageName] -> Action [BuiltLibrary]
askBuiltLibraries = flip forP askBuiltLibrary

askMaybeBuiltLibrary :: PackageName -> Action (Maybe BuiltLibrary)
askMaybeBuiltLibrary pkg = askPersistent (BuiltLibraryQ pkg)

askBuiltLibrary :: PackageName -> Action BuiltLibrary
askBuiltLibrary pkg = askMaybeBuiltLibrary pkg >>= helper
  where
    helper Nothing = error $ "buildFromDesc: " ++ display pkg
                                ++ " does not have a buildable library"
    helper (Just lib) = return lib


data BuiltDeps = BuiltDeps [PackageIdentifier] TransitiveDeps
  deriving Show

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
        Hackage pid flags -> do
            dir <- getPackageSourceDir pid
            Right . addHappyAlexSourceDirs <$> getConfigured conf flags dir
        Local dir _ -> Right <$> getConfigured conf HM.empty dir
  where
    getConfigured :: Config -> Flags -> Artifact -> Action ConfiguredPkg
    getConfigured conf flags dir = do
        (desc, dir') <- configurePackage (plan conf) flags dir
        macros <- genCabalMacros conf desc
        return $ ConfiguredPkg desc dir' macros


buildLibrary :: BuiltLibraryQ -> Action (Maybe BuiltLibrary)
buildLibrary (BuiltLibraryQ pkg) =
    getConfiguredPackage pkg >>= \case
        Left p -> Just . BuiltLibrary p <$> askBuiltinLibrary
                                                (packageIdToUnitId p)
        Right confd
            | Just lib <- library (confdDesc confd)
            , let bi = libBuildInfo lib
            , buildable bi -> Just <$> do
                deps <- askBuiltDeps $ targetDepNames bi
                buildLibraryFromDesc deps confd lib
            | otherwise -> return Nothing
  where
    packageIdToUnitId :: PackageId -> UnitId
    packageIdToUnitId = mkUnitId . display

getBuiltinLib :: BuiltinLibraryR -> Action TransitiveDeps
getBuiltinLib (BuiltinLibraryR p) = do
    ghc <- configGhc <$> askConfig
    result <- runCommandStdout
                $ ghcPkgProg ghc
                    ["describe" , display p]
    info <- case IP.parseInstalledPackageInfo result of
        IP.ParseFailed err -> error (show err)
        IP.ParseOk _ info -> return info
    deps <- mapM askBuiltinLibrary $ IP.depends info
    let paths f = Set.fromList . map (parseGlobalPackagePath ghc)
                        . f $ info
    return $ mconcat deps <> TransitiveDeps
                    { transitiveDBs = Set.empty
                    -- Don't bother tracking compile-time files for built-in
                    -- libraries, since they're already provided implicitly
                    -- by `ghcProg`.
                    , transitiveLibFiles = Set.empty
                    , transitiveIncludeDirs = paths IP.includeDirs
                    -- Make dynamic libraries available at runtime,
                    -- falling back to the regular dir if it's not set
                    -- (usually these will be the same).
                    , transitiveDataFiles = paths IP.libraryDirs
                                            <> paths IP.libraryDynDirs
                    }

askBuiltinLibrary :: UnitId -> Action TransitiveDeps
askBuiltinLibrary = askPersistent . BuiltinLibraryR

newtype BuiltinLibraryR = BuiltinLibraryR UnitId
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)
type instance RuleResult BuiltinLibraryR = TransitiveDeps

instance Show BuiltinLibraryR where
    show (BuiltinLibraryR p) = "Library " ++ display p ++ " (built-in)"


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
    let libHSName = "HS" ++ display (packageName $ package desc)
    let dynLibFile = "lib" ++ libHSName
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
                (hiDir', dynLib) <- runCommand
                    (liftA2 (,) (output hiDir) (output dynLibFile))
                    $ message (display (package desc) ++ ": building library")
                    <> inputList (moduleBootFiles ++ cIncludes)
                    <> ghcCommand ghc deps lbi confd
                            [ "-this-unit-id", display $ package desc
                            , "-hidir", hiDir
                            , "-hisuf", "dyn_hi"
                            , "-osuf", "dyn_o"
                            , "-odir", oDir
                            , "-shared", "-dynamic"
                            , "-o", dynLibFile
                            ]
                            (moduleFiles ++ cFiles)
                return $ Just (libHSName, lib, dynLib, hiDir')
    (pkgDb, libFiles) <- registerPackage ghc (package desc) lbi maybeLib
                deps
    let linkerData = maybe Set.empty (\(_,_,dyn,_) -> Set.singleton dyn)
                        maybeLib
    transInstallIncludes <- collectInstallIncludes packageSourceDir lbi
    return $ BuiltLibrary (package desc)
            $ transDeps <> TransitiveDeps
                { transitiveDBs = Set.singleton pkgDb
                , transitiveLibFiles = Set.singleton libFiles
                , transitiveIncludeDirs =
                        maybe Set.empty Set.singleton transInstallIncludes
                , transitiveDataFiles = linkerData
                        -- TODO: just the lib
                        <> Set.singleton libFiles
                }


-- TODO: double-check no two executables with the same name

data BuiltExecutable = BuiltExecutable
    { builtBinary :: Artifact
    , builtExeDataFiles :: Set Artifact
    } deriving (Show, Eq, Generic, Hashable, Binary, NFData)

progExe :: BuiltExecutable -> [String] -> Command
progExe exe args = progA (builtBinary exe) args
                <> inputs (builtExeDataFiles exe)

newtype BuiltExecutablesQ = BuiltExecutablesQ PackageName
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)
type instance RuleResult BuiltExecutablesQ = Map.Map String BuiltExecutable
instance Show BuiltExecutablesQ where
    show (BuiltExecutablesQ p) = "Executables from " ++ display p

askBuiltExecutables :: PackageName -> Action (Map.Map String BuiltExecutable)
askBuiltExecutables = askPersistent . BuiltExecutablesQ

buildExecutables :: BuiltExecutablesQ -> Action (Map.Map String BuiltExecutable)
buildExecutables (BuiltExecutablesQ p) = getConfiguredPackage p >>= \case
    Left _ -> return Map.empty
    Right confd ->
        fmap Map.fromList
            . mapM (\e -> (display $ exeName e,)
                                <$> buildExecutableFromPkg confd e)
            . filter (buildable . buildInfo)
            $ executables (confdDesc confd)

data BuiltExecutableQ = BuiltExecutableQ PackageName String
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)
type instance RuleResult BuiltExecutableQ = BuiltExecutable

instance Show BuiltExecutableQ where
    show (BuiltExecutableQ p e) = "Executable " ++ e ++ " from " ++ display p

askBuiltExecutable :: PackageName -> String -> Action BuiltExecutable
askBuiltExecutable p e = askPersistent $ BuiltExecutableQ p e

-- TODO: error if not buildable?
buildExecutable :: BuiltExecutableQ -> Action BuiltExecutable
buildExecutable (BuiltExecutableQ p e) = getConfiguredPackage p >>= \case
    Left pid -> error $ "Built-in package " ++ display pid
                        ++ " has no executables"
    Right confd
        | Just exe <- find ((== e) . display . exeName) (executables $ confdDesc confd)
            -> buildExecutableFromPkg confd exe
        | otherwise -> error $ "Package " ++ display (packageId confd)
                            ++ " has no executable named " ++ e

buildExecutableFromPkg
    :: ConfiguredPkg
    -> Executable
    -> Action BuiltExecutable
buildExecutableFromPkg confd exe = do
    let name = display $ exeName exe
    let desc = confdDesc confd
    let packageSourceDir = confdSourceDir confd
    let bi = buildInfo exe
    deps@(BuiltDeps _ transDeps)
        <- askBuiltDeps $ targetDepNamesOrAllDeps desc bi
    conf <- askConfig
    let ghc = configGhc conf
    let cIncludeDirs = transitiveIncludeDirs transDeps
                        <> Set.map (packageSourceDir />)
                                 (Set.fromList $ ifNullDirs $ includeDirs bi)
    datas <- collectDataFiles ghc desc packageSourceDir
    let sourceLocs = sourceDirArtifacts packageSourceDir bi
    otherModuleFiles <- mapM (findModule ghc desc bi cIncludeDirs datas
                                sourceLocs)
                         $ addIfMissing pathsMod $ otherModules bi
    mainFile <- findMainFile ghc bi cIncludeDirs sourceLocs (modulePath exe)
    moduleBootFiles <- catMaybes <$> mapM findBootFile otherModuleFiles
    let cFiles = map (packageSourceDir />) $ cSources bi
    cIncludes <- collectCIncludes desc bi (packageSourceDir />)
    let out = "exe" </> name
    bin <- runCommand (output out)
        $ message (display (package desc) ++ ": building executable "
                    ++ name)
        <> inputList moduleBootFiles
        <> inputList cIncludes
        <> ghcCommand ghc deps bi confd
                [ "-o", out
                , "-hidir", "hi"
                , "-odir", "o"
                , "-dynamic"
                , "-threaded"
                ]
                (addIfMissing mainFile otherModuleFiles ++ cFiles)
    return BuiltExecutable
        { builtBinary = bin
        , builtExeDataFiles = foldr Set.insert (transitiveDataFiles transDeps)
                                    datas
        }
  where
    pathsMod = fromString $ "Paths_" ++ fixDashes (display $ packageName confd)
    addIfMissing m ms
        | m `elem` ms = ms
        | otherwise = m : ms
    fixDashes = map $ \c -> if c == '-' then '_' else c


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
            Just (libHSName, lib, dynLibA, hi) ->
                ( shadow dynLibA (pre </> takeFileName (pathIn dynLibA))
                    <> shadow hi (pre </> "hi")
                , [ "hs-libraries: " ++ libHSName
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
            <> ghcPkgProg ghc ["init", db]
            <> ghcPkgProg ghc
                    (["-v0"]
                    ++ [ "--package-db=" ++ pathIn f
                       | f <-  Set.toList $ transitiveDBs transDeps
                       ]
                    ++ ["--package-db", db, "register",
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
    maybe (error $ "Missing module " ++ display m
                    ++ "; searched " ++ show paths)
        return found

findMainFile
    :: InstalledGhc
    -> BuildInfo
    -> Set Artifact -- ^ Transitive C include dirs
    -> [Artifact]  -- ^ Source directory to check
    -> FilePath
    -> Action Artifact
findMainFile ghc bi cIncludeDirs paths f = do
    found <- runMaybeT $ msum $
                map findFileDirectly paths ++
                map (search ghc bi cIncludeDirs $ filePathToModule f) paths
    maybe (error $ "Missing main file " ++ f
                    ++ "; searched " ++ show paths)
        return found
  where
    findFileDirectly path = do
        let candidate = path /> f
        exists candidate
        return candidate

genPathsModule
    :: ModuleName -> PackageIdentifier -> Maybe Artifact -> MaybeT Action Artifact
genPathsModule m pkg datas = do
    guard $ m == pathsModule
    lift $ writeArtifact ("paths" </> display m <.> "hs") $ unlines
        [ "{-# LANGUAGE CPP #-}"
        , "{-# LANGUAGE ImplicitPrelude #-}"
        , "module " ++ display m ++ " (getDataFileName, getDataDir, version) where"
        , "import Data.Version (Version(..))"
        , "version = Version " ++ show (versionNumbers
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
      genC2hs <|>
      existing "lhs" <|>
      existing "hs"
  where
    genHappy ext = do
        let yFile = srcDir /> toFilePath m <.> ext
        exists yFile
        let relOutput = toFilePath m <.> "hs"
        happy <- lift $ askBuiltExecutable (mkPackageName "happy") "happy"
        lift . runCommand (output relOutput)
             $ progExe happy
                     ["-o", relOutput, pathIn yFile]
                <> input yFile

    genHsc2hs = do
        let hsc = srcDir /> toFilePath m <.> "hsc"
        exists hsc
        let relOutput = toFilePath m <.> "hs"
        lift $ runCommand (output relOutput)
             $ hsc2hsProg ghc
                      (["-o", relOutput
                       , pathIn hsc
                       ]
                       -- TODO: CPP options?
                       ++ ["--cflag=" ++ f | f <- ccOptions bi
                                                    ++ cppOptions bi]
                       ++ ["-I" ++ pathIn f | f <- Set.toList cIncludeDirs]
                       ++ ["-D__GLASGOW_HASKELL__="
                             ++ cppVersion (ghcInstalledVersion ghc)])
                <> input hsc <> inputs cIncludeDirs

    genAlex ext = do
        let xFile = srcDir /> toFilePath m <.> ext
        exists xFile
        let relOutput = toFilePath m <.> "hs"
        -- TODO: mkPackageName doesn't exist in older ones
        alex <- lift $ askBuiltExecutable (mkPackageName "alex") "alex"
        lift . runCommand (output relOutput)
            $ progExe alex
                     ["-o", relOutput, pathIn xFile]
               <> input xFile
    genC2hs = do
        let chsFile = srcDir /> toFilePath m <.> "chs"
        exists chsFile
        let relOutput = toFilePath m <.> "hs"
        c2hs <- lift $ askBuiltExecutable (mkPackageName "c2hs") "c2hs"
        lift . runCommand (output relOutput)
             $ input chsFile
            <> progExe c2hs
                    (["-o", relOutput, pathIn chsFile]
                    ++ ["--include=" ++ pathIn f | f <- Set.toList cIncludeDirs]
                    ++ ["--cppopts=" ++ f | f <- ccOptions bi
                                                    ++ cppOptions bi]
                    )

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
    extraSrcs <- fmap concat $ mapM (matchArtifactGlob (pkgDir ""))
                            $ extraSrcFiles desc
    extraTmps <- fmap catMaybes . mapM ((\f -> doesArtifactExist f >>= \case
                                                True -> return (Just f)
                                                False -> return Nothing)
                                        . pkgDir)
                        $ extraTmpFiles desc
    return $ includeInputs ++ map (pkgDir "" />) extraSrcs ++ extraTmps

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
    if null (dataFiles desc)
        then return Nothing
        else Just <$> do
            files <- concat <$> mapM (matchArtifactGlob dir) (dataFiles desc)
            groupFiles inDir . map (\x -> (x,x)) $ files

collectInstallIncludes :: Artifact -> BuildInfo -> Action (Maybe Artifact)
collectInstallIncludes dir bi
    | null (installIncludes bi) = pure Nothing
    | otherwise = fmap Just (mapM locateHeader (installIncludes bi)
                                >>= groupFiles dir)
  where
    -- | Returns the actual location of that header (potentially in some includeDir)
    -- paired with the original name of that header without the dir.
    locateHeader :: FilePath -> Action (FilePath, FilePath)
    locateHeader f = do
        let candidates = map (\d -> (d, dir /> d </> f)) ("" : includeDirs bi)
        existing <- filterM (doesArtifactExist . snd) candidates
        case existing of
            (d, _):_ -> return (d </> f, f)
            _ -> error $ "Couldn't locate install-include " ++ show f

-- | Group source files by shadowing into a single directory.
groupFiles :: Artifact -> [(FilePath, FilePath)] -> Action Artifact
groupFiles dir files = let out = "group"
                   in runCommand (output out)
                        $ prog "mkdir" ["-p", out]
                        <> foldMap (\(f, g) -> shadow (dir /> f) (out </> g))
                            files

cppVersion :: Version -> String
cppVersion v = case versionNumbers v of
    (v1:v2:_) -> show v1 ++ if v2 < 10 then '0':show v2 else show v2
    _ -> error $ "cppVersion: " ++ display v

exists :: Artifact -> MaybeT Action ()
exists f = lift (doesArtifactExist f) >>= guard

data ConfiguredPkg = ConfiguredPkg
    { confdDesc :: PackageDescription
    , confdSourceDir :: Artifact
    , confdMacros :: Artifact
        -- ^ Provides Cabal macros like VERSION_*
    }

instance Package ConfiguredPkg where
    packageId = packageId . confdDesc

-- For compatibility with Cabal, we generate a single macros file for the
-- entire package, rather than separately for the library, executables, etc.
-- For example, `pandoc-1.19.2.1`'s `pandoc` executable references
-- `VERSION_texmath` in `pandoc.hs`, despite not directly depending on the
-- `texmath` package.
genCabalMacros :: Config -> PackageDescription -> Action Artifact
genCabalMacros conf =
    writeArtifact "macros.h"
        . generatePackageVersionMacros
        . map (resolvedPackageId . resolvePackage conf)
        . allDependencies

targetDepNames :: BuildInfo -> [PackageName]
targetDepNames bi = [n | Dependency n _ <- targetBuildDepends bi]

allDependencies :: PackageDescription -> [PackageName]
allDependencies desc = let
    allBis = [libBuildInfo l | Just l <- [library desc]]
                    ++ map buildInfo (executables desc)
                    ++ map testBuildInfo (testSuites desc)
                    ++ map benchmarkBuildInfo (benchmarks desc)
   in Set.toList . Set.fromList . concatMap targetDepNames $ allBis

-- | In older versions of Cabal, executables could use packages that were only
-- explicitly depended on in the library or in other executables.  Some existing
-- packages still assume this behavior.
targetDepNamesOrAllDeps :: PackageDescription -> BuildInfo -> [PackageName]
targetDepNamesOrAllDeps desc bi
    | specVersion desc >= mkVersion [1,8] = targetDepNames bi
    | otherwise = maybe [] (const [packageName desc]) (library desc)
                    ++ allDependencies desc

addHappyAlexSourceDirs :: ConfiguredPkg -> ConfiguredPkg
addHappyAlexSourceDirs confd
    | packageName (confdDesc confd) `elem` map mkPackageName ["happy", "alex"]
        = confd { confdDesc = addDistSourceDirs $ confdDesc confd }
    | otherwise = confd
