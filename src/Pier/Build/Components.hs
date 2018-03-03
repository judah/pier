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
import Data.List (find, nub)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup
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
import Distribution.Version (mkVersion)
import GHC.Generics hiding (packageName)
import Language.Haskell.Extension

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Distribution.InstalledPackageInfo as IP

import Pier.Build.Config
import Pier.Build.Custom
import Pier.Build.Executable
import Pier.Build.Flags
import Pier.Build.Module
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
    let cFiles = map pkgDir $ cSources lbi
    datas <- collectDataFiles ghc desc packageSourceDir
    cflags <- getCFlags transDeps packageSourceDir lbi
    moduleFiles <- mapM (findModule ghc desc cflags datas
                            $ sourceDirArtifacts packageSourceDir lbi)
                        modules
    moduleBootFiles <- catMaybes <$> mapM findBootFile moduleFiles
    cIncludes <- collectCIncludes desc lbi pkgDir
    extra <- getExtraSrcFiles confd
    maybeLib <- if not shouldBuildLib
            then return Nothing
            else do
                (hiDir', dynLib) <- runCommand
                    (liftA2 (,) (output hiDir) (output dynLibFile))
                    $ message (display (package desc) ++ ": building library")
                    <> inputList (moduleBootFiles ++ cIncludes)
                    <> ghcCommand ghc deps lbi confd extra cflags
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
    (pkgDb, libFiles) <- registerPackage ghc (package desc) lbi cflags maybeLib
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
    datas <- collectDataFiles ghc desc packageSourceDir
    let sourceLocs = sourceDirArtifacts packageSourceDir bi
    cflags <- getCFlags transDeps packageSourceDir bi
    otherModuleFiles <- mapM (findModule ghc desc cflags datas sourceLocs)
                         $ addIfMissing pathsMod $ otherModules bi
    mainFile <- findMainFile ghc cflags sourceLocs (modulePath exe)
    moduleBootFiles <- catMaybes <$> mapM findBootFile otherModuleFiles
    let cFiles = map (packageSourceDir />) $ cSources bi
    cIncludes <- collectCIncludes desc bi (packageSourceDir />)
    let out = "exe" </> name
    extra <- getExtraSrcFiles confd
    bin <- runCommand (output out)
        $ message (display (package desc) ++ ": building executable "
                    ++ name)
        <> inputList moduleBootFiles
        <> inputList cIncludes
        <> ghcCommand ghc deps bi confd extra cflags
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


ghcCommand
    :: InstalledGhc
    -> BuiltDeps
    -> BuildInfo
    -> ConfiguredPkg
    -> ExtraSrcFiles
    -> CFlags
    -> [String]
    -> [Artifact]
    -> Command
ghcCommand ghc (BuiltDeps depPkgs transDeps) bi confd extraSrcs cflags
    args ghcInputs
        = shadowExtraSrcFiles extraSrcs
            <> ghcProg ghc (allArgs ++ map pathIn ghcInputs)
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
    allArgs =
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
        -- Include files which are sources
        ++ map (("-I" ++) . pathIn . pkgDir) (includeDirs bi)
        -- Include files which are listed as extra-src-files, and thus shadowed directly into
        -- the working dir:
        ++ map ("-I" ++) (includeDirs bi)
        ++ map ("-X" ++) extensions
        ++ concat [opts | (GHC,opts) <- options bi]
        ++ map ("-optP" ++) (cppFlags cflags)
        ++ ["-optP-include", "-optP" ++ pathIn (confdMacros confd)]
        ++ ["-optc" ++ opt | opt <- ccFlags cflags]
        ++ ["-l" ++ libDep | libDep <- linkLibs cflags]
        ++ ["-optl" ++ f | f <- linkLibs cflags]
        -- TODO: configurable
        ++ ["-O0"]
        -- TODO: just for local builds
        ++ ["-w"]
        ++ args

sourceDirArtifacts :: Artifact -> BuildInfo -> [Artifact]
sourceDirArtifacts packageSourceDir bi
    = map (packageSourceDir />) $ ifNullDirs $ hsSourceDirs bi

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
                  ]
                )
    spec <- writeArtifact "spec" $ unlines $
        [ "name: " ++ display (packageName pkg)
        , "version: " ++ display (packageVersion pkg)
        , "id: " ++ display pkg
        , "key: " ++ display pkg
        , "extra-libraries: " ++ unwords (linkLibs cflags)
        , "ld-options: " ++ unwords (linkFlags cflags)
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
ifNullDirs :: [FilePath] -> [FilePath]
ifNullDirs [] = [""]
ifNullDirs xs = xs

newtype ExtraSrcFiles = ExtraSrcFiles { shadowExtraSrcFiles :: Command }

-- Embed extra-source-files two ways: as regular inputs, and shadowed
-- directly into the working directory.
-- They're needed as regular inputs so that, if they're headers, they stay next
-- to c-sources (which the C include system expects).
-- They're needed directly in the working directory to be available to template
-- haskell splices.
getExtraSrcFiles :: ConfiguredPkg -> Action ExtraSrcFiles
getExtraSrcFiles pkg
    = do
        files <- fmap (nub . concat)
                    . mapM (matchArtifactGlob (confdSourceDir pkg))
                    . extraSrcFiles
                    $ confdDesc pkg
        return . ExtraSrcFiles $
            inputList (map (confdSourceDir pkg />) files)
            <> foldMap (\f -> shadow (confdSourceDir pkg /> f) f)
                  files

collectCIncludes :: PackageDescription -> BuildInfo -> (FilePath -> Artifact) -> Action [Artifact]
collectCIncludes desc bi pkgDir = do
    includeInputs <- findIncludeInputs pkgDir bi
    extraTmps <- fmap catMaybes . mapM ((\f -> doesArtifactExist f >>= \case
                                                True -> return (Just f)
                                                False -> return Nothing)
                                        . pkgDir)
                        $ extraTmpFiles desc
    return $ includeInputs ++ extraTmps

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
