{-# LANGUAGE DeriveAnyClass #-}
module Pier.Build.Components
    ( buildPackageRules
    , askBuiltLibrary
    , askMaybeBuiltLibrary
    , askBuiltExecutables
    , askBuiltExecutable
    , BuiltExecutable(..)
    )
    where

import Control.Applicative (liftA2)
import Control.Monad (filterM)
import Data.List (find, intercalate)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath hiding (exe)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.System (buildOS, OS(..))
import Distribution.Text
import Distribution.Version (mkVersion)
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
                (hiDir', dynLib) <- runCommand
                    (liftA2 (,) (output hiDir) (output dynLibFile))
                    $ message (display pkg ++ ": building library")
                    <> ghcCommand ghc deps confd tinfo
                            [ "-this-unit-id", display pkg
                            , "-hidir", hiDir
                            , "-hisuf", "dyn_hi"
                            , "-osuf", "dyn_o"
                            , "-odir", oDir
                            , "-shared", "-dynamic"
                            , "-o", dynLibFile
                            ]
                return $ Just (libHSName, lib, dynLib, hiDir')
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
    :: ConfiguredPackage
    -> Executable
    -> Action BuiltExecutable
buildExecutableFromPkg confd exe = do
    let name = display $ exeName exe
    let desc = confdDesc confd
    deps@(BuiltDeps _ transDeps)
        <- askBuiltDeps $ targetDepNamesOrAllDeps desc (buildInfo exe)
    ghc <- configGhc <$> askConfig
    let out = "exe" </> name
    tinfo <- getTargetInfo confd (buildInfo exe) (TargetBinary $ modulePath exe)
                transDeps ghc
    bin <- runCommand (output out)
        $ message (display (package desc) ++ ": building executable "
                    ++ name)
        <> ghcCommand ghc deps confd tinfo
                [ "-o", out
                , "-hidir", "hi"
                , "-odir", "o"
                , "-dynamic"
                , "-threaded"
                ]
    return BuiltExecutable
        { builtBinary = bin
        , builtExeDataFiles = foldr Set.insert (transitiveDataFiles transDeps)
                                (confdDataFiles confd)
        }

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
        <> input (confdMacros confd)
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
        ++ ["-optP-include", "-optP" ++ pathIn (confdMacros confd)]
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
                  , "reexported-modules: " ++
                        intercalate ", " (map display $ reexportedModules lib)
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

-- | In older versions of Cabal, executables could use packages that were only
-- explicitly depended on in the library or in other executables.  Some existing
-- packages still assume this behavior.
targetDepNamesOrAllDeps :: PackageDescription -> BuildInfo -> [PackageName]
targetDepNamesOrAllDeps desc bi
    | specVersion desc >= mkVersion [1,8] = targetDepNames bi
    | otherwise = maybe [] (const [packageName desc]) (library desc)
                    ++ allDependencies desc

