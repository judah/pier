{-# LANGUAGE DeriveAnyClass #-}
module Pier.Build.Components
    ( buildPackageRules
    , askBuiltLibrary
    , askMaybeBuiltLibrary
    , askBuiltExecutables
    , askBuiltExecutable
    , askBuiltTestSuite
    , askBuiltTestSuites
    , BuiltBinary(..)
    )
    where

import Control.Monad (filterM, (>=>))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath hiding (exe)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.System (buildOS, OS(..))
import Distribution.Text
import GHC.Generics hiding (packageName)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Distribution.InstalledPackageInfo as IP

import Pier.Build.Config
import Pier.Build.ConfiguredPackage
import Pier.Build.Executable
import Pier.Build.CFlags
import Pier.Build.Stackage
import Pier.Build.TargetInfo
import Pier.Core.Artifact
import Pier.Core.Persistent


buildPackageRules :: Rules ()
buildPackageRules = do
    addPersistent buildLibrary
    addPersistent getBuiltinLib
    addPersistent buildExecutables
    addPersistent buildExecutable
    addPersistent buildTestSuites
    addPersistent buildTestSuite

newtype BuiltLibraryQ = BuiltLibraryQ PackageName
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)
type instance RuleResult BuiltLibraryQ = Maybe BuiltLibrary

instance Show BuiltLibraryQ where
    show (BuiltLibraryQ p) = "Library " ++ display p


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
    conf <- askConfig
    let ghc = configGhc conf
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
    -> ConfiguredPackage
    -> Library
    -> Action BuiltLibrary
buildLibraryFromDesc deps@(BuiltDeps _ transDeps) confd lib = do
    let pkg = package $ confdDesc confd
    conf <- askConfig
    let ghc = configGhc conf
    let lbi = libBuildInfo lib
    tinfo <- getTargetInfo confd lbi (TargetLibrary $ exposedModules lib)
                transDeps ghc
    maybeLib <- if null $ exposedModules lib
            then return Nothing
            else do
                let hiDir = "hi"
                let oDir = "o"
                let libHSName = "HS" ++ display (packageName pkg)
                let dynLibFile = "lib" ++ libHSName
                                    ++ "-ghc" ++ display (ghcVersion $ plan conf)
                                    <.> dynExt
                ds <- runCommand
                    $ message (display pkg ++ ": building library")
                    <> ghcCommand ghc deps confd tinfo
                          (ghcOptions conf ++
                            [ "-this-unit-id", display pkg
                            , "-hidir", hiDir
                            , "-hisuf", "dyn_hi"
                            , "-osuf", "dyn_o"
                            , "-odir", oDir
                            , "-shared", "-dynamic"
                            , "-o", dynLibFile
                            ])
                return $ Just (libHSName, lib, ds /> dynLibFile, ds /> hiDir)
    (pkgDb, libFiles) <- registerPackage ghc pkg lbi
                                (targetCFlags tinfo) maybeLib
                                deps
    let linkerData = maybe Set.empty (\(_,_,dyn,_) -> Set.singleton dyn)
                        maybeLib
    transInstallIncludes <- collectInstallIncludes (confdSourceDir confd) lbi
    return $ BuiltLibrary pkg
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

newtype BuiltExecutablesQ = BuiltExecutablesQ PackageName
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)
type instance RuleResult BuiltExecutablesQ = [BuiltBinary]
instance Show BuiltExecutablesQ where
    show (BuiltExecutablesQ p) = "Executables from " ++ display p

askBuiltExecutables :: PackageName -> Action [BuiltBinary]
askBuiltExecutables = askPersistent . BuiltExecutablesQ

data BuiltTestSuiteQ = BuiltTestSuiteQ PackageName String
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)
type instance RuleResult BuiltTestSuiteQ = BuiltBinary

instance Show BuiltTestSuiteQ where
    show (BuiltTestSuiteQ p s) = "TestSuite " ++ s ++ " from " ++ display p

askBuiltTestSuite :: PackageName -> String -> Action BuiltBinary
askBuiltTestSuite p e = askPersistent $ BuiltTestSuiteQ p e

buildExecutables :: BuiltExecutablesQ -> Action [BuiltBinary]
buildExecutables (BuiltExecutablesQ p) = getConfiguredPackage p >>= \case
    Left _ -> return []
    Right confd ->
            mapM (buildBinaryFromPkg confd . exeSpec)
            . filter (buildable . buildInfo)
            $ executables (confdDesc confd)

-- TODO: error if not buildable?
buildExecutable :: BuiltExecutableQ -> Action BuiltBinary
buildExecutable (BuiltExecutableQ p e) = getConfiguredPackage p >>= \case
    Left pid -> error $ "Built-in package " ++ display pid
                        ++ " has no executables"
    Right confd
        | Just exe <- find ((== e) . display . exeName) (executables $ confdDesc confd)
            -> buildBinaryFromPkg confd (exeSpec exe)
        | otherwise -> error $ "Package " ++ display (packageId confd)
                            ++ " has no executable named " ++ e

data BinarySpec = BinarySpec
    { binaryTypeName :: String
    , binaryName :: String
    , binaryPath :: FilePath
    , binaryBuildInfo :: BuildInfo
    }

exeSpec :: Executable -> BinarySpec
exeSpec e = BinarySpec
                { binaryTypeName = "executable"
                , binaryName = display $ exeName e
                , binaryPath = modulePath e
                , binaryBuildInfo = buildInfo e
                }

testSpec :: TestSuite -> Action BinarySpec
testSpec t@TestSuite { testInterface = TestSuiteExeV10 _ path }
    = return BinarySpec
                { binaryTypeName = "test-suite"
                , binaryName = display $ testName t
                , binaryPath = path
                , binaryBuildInfo = testBuildInfo t
                }
testSpec t = fail $ "Unknown test type " ++ show (testInterface t)
                    ++ " for test " ++ display (testName t)

buildBinaryFromPkg
    :: ConfiguredPackage
    -> BinarySpec
    -> Action BuiltBinary
buildBinaryFromPkg confd bin = do
    let desc = confdDesc confd
    deps@(BuiltDeps _ transDeps)
        <- askBuiltDeps $ exeDepNames desc (binaryBuildInfo bin)
    conf <- askConfig
    let ghc = configGhc conf
    let out = "bin" </> binaryName bin
    tinfo <- getTargetInfo confd (binaryBuildInfo bin) (TargetBinary $ binaryPath bin)
                transDeps ghc
    result <- fmap (/> out) $ runCommand
        $ message (display (package desc) ++ ": building "
                        ++ binaryTypeName bin ++ " " ++ binaryName bin)
        <> mkdir (takeDirectory out)
        <> ghcCommand ghc deps confd tinfo
              (ghcOptions conf ++
                [ "-o", out
                , "-hidir", "hi"
                , "-odir", "o"
                , "-dynamic"
                , "-threaded"
                ])
    return BuiltBinary
        { builtBinary = result
        , builtBinaryDataFiles = foldr Set.insert (transitiveDataFiles transDeps)
                                (confdDataFiles confd)
        }

newtype BuiltTestSuitesQ = BuiltTestSuitesQ PackageName
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)
type instance RuleResult BuiltTestSuitesQ = [BuiltBinary]
instance Show BuiltTestSuitesQ where
    show (BuiltTestSuitesQ p) = "Test suites from " ++ display p

askBuiltTestSuites :: PackageName -> Action [BuiltBinary]
askBuiltTestSuites = askPersistent . BuiltTestSuitesQ

buildTestSuites :: BuiltTestSuitesQ -> Action [BuiltBinary]
buildTestSuites (BuiltTestSuitesQ p) = getConfiguredPackage p >>= \case
    Left _ -> return []
    Right confd ->
            mapM (testSpec >=> buildBinaryFromPkg confd)
            . filter (buildable . testBuildInfo)
            $ testSuites (confdDesc confd)

-- TODO: error if not buildable?
buildTestSuite :: BuiltTestSuiteQ -> Action BuiltBinary
buildTestSuite (BuiltTestSuiteQ p s) = getConfiguredPackage p >>= \case
    Left pid -> error $ "Built-in package " ++ display pid
                        ++ " has no test suites"
    Right confd
        | Just suite <-
            find ((== s) . display . testName) (testSuites $ confdDesc confd)
            -> testSpec suite >>= buildBinaryFromPkg confd
        | otherwise -> error $ "Package " ++ display (packageId confd)
                            ++ " has no test suite named " ++ s

ghcCommand
    :: InstalledGhc
    -> BuiltDeps
    -> ConfiguredPackage
    -> TargetInfo
    -> [String]
    -> Command
ghcCommand ghc (BuiltDeps depPkgs transDeps) confd tinfo args
    = inputs (transitiveDBs transDeps)
        <> inputs (transitiveLibFiles transDeps)
        <> inputList (targetSourceInputs tinfo ++ targetOtherInputs tinfo)
        -- Embed extra-source-files two ways: as regular inputs, and shadowed
        -- directly into the working directory.
        -- They're needed as regular inputs so that, if they're headers, they
        -- stay next to c-sources (which the C include system expects).
        -- They're needed directly in the working directory to be available to
        -- template haskell splices.
        <> inputList (map pkgFile $ confdExtraSrcFiles confd)
        <> foldMap (\f -> shadow (pkgFile f) f) (confdExtraSrcFiles confd)
        <> ghcProg ghc (allArgs ++ map pathIn (targetSourceInputs tinfo))
  where
    cflags = targetCFlags tinfo
    pkgFile = (confdSourceDir confd />)
    allArgs =
        -- Rely on GHC for module ordering and hs-boot files:
        [ "--make"
        , "-v0"
        , "-fPIC"
        , "-i"
        ]
        -- Necessary for boot files:
        ++ map (("-i" ++) . pathIn) (targetSourceDirs tinfo)
        ++
        concatMap (\p -> ["-package-db", pathIn p])
                (Set.toList $ transitiveDBs transDeps)
        ++
        concat [["-package", display d] | d <- depPkgs]
        -- Include files which are sources
        ++ map (("-I" ++) . pathIn . pkgFile) (targetIncludeDirs tinfo)
        -- Include files which are listed as extra-src-files, and thus shadowed directly into
        -- the working dir:
        ++ map ("-I" ++) (targetIncludeDirs tinfo)
        ++ targetOptions tinfo
        ++ map ("-optP" ++) (cppFlags cflags)
        ++ ["-optc" ++ opt | opt <- ccFlags cflags]
        ++ ["-l" ++ libDep | libDep <- linkLibs cflags]
        ++ ["-optl" ++ f | f <- linkFlags cflags]
        ++ concat [["-framework", f] | f <- macFrameworks cflags]
        -- TODO: configurable
        ++ ["-O0"]
        -- TODO: just for local builds
        ++ ["-w"]
        ++ args

registerPackage
    :: InstalledGhc
    -> PackageIdentifier
    -> BuildInfo
    -> CFlags
    -> Maybe ( String  -- Library name for linking
             , Library
             , Artifact -- dyn lib archive
             , Artifact -- hi
             )
    -> BuiltDeps
    -> Action (Artifact, Artifact)
registerPackage ghc pkg bi cflags maybeLib (BuiltDeps depPkgs transDeps)
    = do
    let pre = "files"
    let depsByName = Map.fromList [(packageName p, p) | p <- depPkgs]
    let (collectLibInputs, libDesc) = case maybeLib of
            Nothing -> (createDirectoryA pre, [])
            Just (libHSName, lib, dynLibA, hi) ->
                ( shadow dynLibA (pre </> takeFileName (pathIn dynLibA))
                    <> shadow hi (pre </> "hi")
                , [ "hs-libraries: " ++ libHSName
                  , "library-dirs: ${pkgroot}" </> pre
                  , "dynamic-library-dirs: ${pkgroot}" </> pre
                  , "import-dirs: ${pkgroot}" </> pre </> "hi"
                  , "exposed-modules: " ++
                        unwords (map display (exposedModules lib)
                                ++ map (renderReexport depsByName)
                                        (reexportedModules lib))
                  , "hidden-modules: " ++ unwords (map display $ otherModules bi)
                  ]
                )
    spec <- writeArtifact "spec" $ unlines $
        [ "name: " ++ display (packageName pkg)
        , "version: " ++ display (packageVersion pkg)
        , "id: " ++ display pkg
        , "key: " ++ display pkg
        , "extra-libraries: " ++ unwords (linkLibs cflags)
        -- TODO: this list should be string-separated, and make sure
        -- to quote flags that contain strings (e.g. "-Wl,-E" from hslua).
        -- , "ld-options: " ++ unwords (linkFlags cflags)
        , "depends: " ++ unwords (map display depPkgs)
        ]
        ++ [ "frameworks: " ++ unwords (macFrameworks cflags)
           | not (null $ macFrameworks cflags)
           ]
        ++ libDesc
    let db = display pkg
    fmap (\f -> (f /> db, f /> pre))
        $ runCommand
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

renderReexport ::
    Map.Map PackageName PackageIdentifier -> ModuleReexport -> String
renderReexport deps re = display (moduleReexportName re) ++ " from "
                    ++ maybe "" (\pkg -> display (originalPkg pkg) ++ ":")
                            (moduleReexportOriginalPackage re)
                    ++ display (moduleReexportOriginalName re)
  where
    originalPkg p =
        fromMaybe (error $ "Unknown package name " ++ display p
                                ++ " for module reexport " ++ display re)
            $ Map.lookup p deps

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
