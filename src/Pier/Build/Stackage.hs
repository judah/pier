{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Pier.Build.Stackage
    ( buildPlanRules
    , askBuildPlan
    , askInstalledGhc
    , installGhcRules
    , InstalledGhc(..)
    , GhcDistro(..)
    , ghcArtifacts
    , ghcProg
    , ghcPkgProg
    , hsc2hsProg
    , parseGlobalPackagePath
    , PlanName(..)
    , BuildPlan(..)
    , PlanPackage(..)
    , Flags
    ) where

import Control.Exception (throw)
import Data.Binary.Orphans ()
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Yaml
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Distribution.Package
import Distribution.PackageDescription (FlagName)
import Distribution.System (buildPlatform, Platform(..), Arch(..), OS(..))
import Distribution.Version
import GHC.Generics

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Distribution.Text as Cabal

import Pier.Core.Artifact
import Pier.Core.Download
import Pier.Core.Persistent
import Pier.Orphans ()

newtype PlanName = PlanName { renderPlanName :: String }
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)

instance FromJSON PlanName where
    parseJSON = fmap PlanName . parseJSON

data BuildPlan = BuildPlan
    { corePackageVersions :: HM.HashMap PackageName Version
    , planPackages :: HM.HashMap PackageName PlanPackage
    , ghcVersion :: Version
    } deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)

data PlanPackage = PlanPackage
    { planPackageVersion :: Version
    , planPackageFlags :: Flags
    } deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)

type Flags = HM.HashMap FlagName Bool

instance FromJSON BuildPlan where
    parseJSON = withObject "Plan" $ \o -> do
        sys <- o .: "system-info"
        coreVersions <- sys .: "core-packages"
        ghcVers <- sys .: "ghc-version"
        pkgs <- o .: "packages"
        return BuildPlan { corePackageVersions = coreVersions
                         , planPackages = pkgs
                         , ghcVersion = ghcVers
        }

instance FromJSON PlanPackage where
    parseJSON = withObject "PlanPackage" $ \o ->
        PlanPackage <$> (o .: "version") <*> ((o .: "constraints") >>= (.: "flags"))

buildPlanRules :: Rules ()
buildPlanRules = addPersistent $ \(ReadPlan planName) -> do
        f <- askDownload Download
                { downloadFilePrefix = "stackage/plan"
                , downloadName = renderPlanName planName <.> "yaml"
                , downloadUrlPrefix = planUrlPrefix planName
                }
        cs <- readArtifactB f
        case decodeEither' cs of
            Left err -> throw err
            Right x -> return x

planUrlPrefix :: PlanName -> String
planUrlPrefix (PlanName name)
    | "lts-" `List.isPrefixOf` name = ltsBuildPlansUrl
    | "nightly-" `List.isPrefixOf` name = nightlyBuildPlansUrl
    | otherwise = error $ "Unrecognized plan name " ++ show name
  where
    ltsBuildPlansUrl = "https://raw.githubusercontent.com/fpco/lts-haskell/master/"
    nightlyBuildPlansUrl = "https://raw.githubusercontent.com/fpco/stackage-nightly/master/"

newtype ReadPlan = ReadPlan PlanName
    deriving (Typeable,Eq,Hashable,Binary,NFData,Generic)
type instance RuleResult ReadPlan = BuildPlan

instance Show ReadPlan where
    show (ReadPlan p) = "Read build plan: " ++ renderPlanName p

askBuildPlan :: PlanName -> Action BuildPlan
askBuildPlan = askPersistent . ReadPlan


data InstalledGhcQ = InstalledGhcQ GhcDistro Version [PackageName]
    deriving (Typeable, Eq, Hashable, Binary, NFData, Generic)

data GhcDistro
    = SystemGhc
    | StackageGhc
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

instance Show InstalledGhcQ where
    show (InstalledGhcQ d v pn) = "GHC"
                            ++ " " ++ show d
                            ++ ", version " ++ Cabal.display v
                            ++ ", built-in packages "
                            ++ List.intercalate ", " (map Cabal.display pn)

-- | TODO: make the below functions that use Version take InstalledGhc directly instead
data InstalledGhc = InstalledGhc
    { ghcLibRoot :: Artifact
    , ghcInstalledVersion :: Version
    } deriving (Show, Typeable, Eq, Generic)
instance Hashable InstalledGhc
instance Binary InstalledGhc
instance NFData InstalledGhc

type instance RuleResult InstalledGhcQ = InstalledGhc

globalPackageDb :: InstalledGhc -> Artifact
globalPackageDb ghc = ghcLibRoot ghc /> packageConfD

packageConfD :: String
packageConfD = "package.conf.d"

ghcArtifacts :: InstalledGhc -> Set.Set Artifact
ghcArtifacts g = Set.fromList [ghcLibRoot g]

askInstalledGhc :: BuildPlan -> GhcDistro -> Action InstalledGhc
askInstalledGhc plan distro
    = askPersistent $ InstalledGhcQ distro (ghcVersion plan)
                    $ HM.keys $ corePackageVersions plan

-- | Convert @${pkgroot}@ prefixes, for utilities like hsc2hs that don't
-- see packages directly
--
parseGlobalPackagePath :: InstalledGhc -> FilePath -> Artifact
parseGlobalPackagePath ghc f
    | Just f' <- List.stripPrefix "${pkgroot}/" f
        = ghcLibRoot ghc /> f'
    | otherwise = externalFile f

ghcBinDir :: InstalledGhc -> Artifact
ghcBinDir ghc = ghcLibRoot ghc /> "bin"

ghcProg :: InstalledGhc -> [String] -> Command
ghcProg ghc args =
    progA (ghcBinDir ghc /> "ghc")
        (["-B" ++ pathIn (ghcLibRoot ghc)
         , "-clear-package-db"
         , "-hide-all-packages"
         , "-package-db=" ++ pathIn (globalPackageDb ghc)
         ] ++ args)
        <> input (ghcLibRoot ghc)
        <> input (globalPackageDb ghc)

ghcPkgProg :: InstalledGhc -> [String] -> Command
ghcPkgProg ghc args =
    progA (ghcBinDir ghc /> "ghc-pkg")
        ([ "--global-package-db=" ++ pathIn (globalPackageDb ghc)
         , "--no-user-package-db"
         , "--no-user-package-conf"
         ] ++ args)
        <> input (ghcLibRoot ghc)
        <> input (globalPackageDb ghc)

hsc2hsProg :: InstalledGhc -> [String] -> Command
hsc2hsProg ghc args =
    progA (ghcBinDir ghc /> "hsc2hs")
        (("--template=${TMPDIR}/" ++ pathIn template) : args)
    <> input template
  where
    template = ghcLibRoot ghc /> "template-hsc.h"

installGhcRules :: Rules ()
installGhcRules = addPersistent installGhc

installGhc :: InstalledGhcQ -> Action InstalledGhc
installGhc (InstalledGhcQ distro version corePkgs) = do
    installed <- case distro of
                    StackageGhc -> downloadAndInstallGHC version
                    SystemGhc -> getSystemGhc version
    fixed <- makeRelativeGlobalDb corePkgs installed
    runCommand_ $ ghcPkgProg fixed ["check"]
    return fixed

getSystemGhc :: Version -> Action InstalledGhc
getSystemGhc version = do
    path <- fmap (head . words) . runCommandStdout
                $ prog (versionedGhc version) ["--print-libdir"]
    return $ InstalledGhc (externalFile path) version

data DownloadInfo = DownloadInfo
    { downloadUrl :: String
    -- TODO: use these
    , _contentLength :: Int
    , _sha1 :: String
    }

instance FromJSON DownloadInfo where
    parseJSON = withObject "DownloadInfo" $ \o ->
        DownloadInfo <$> o .: "url"
                     <*> o .: "content-length"
                     <*> o .: "sha1"

-- TODO: multiple OSes, configure-env

newtype StackSetup = StackSetup { ghcVersions :: HM.HashMap Version DownloadInfo }

instance FromJSON StackSetup where
    parseJSON = withObject "StackSetup" $ \o -> do
        ghc <- o .: "ghc"
        StackSetup <$> (ghc .: platformKey)

-- TODO: make this more configurable (eventually, using
-- `LocalBuildInfo.hostPlatform` to help support cross-compilation)
platformKey :: Text
platformKey = case buildPlatform of
    Platform I386   Linux   -> "linux32"
    Platform X86_64 Linux   -> "linux64"
    Platform I386   OSX     -> "macosx"
    Platform X86_64 OSX     -> "macosx"
    Platform I386   FreeBSD -> "freebsd32"
    Platform X86_64 FreeBSD -> "freebsd64"
    Platform I386   OpenBSD -> "openbsd32"
    Platform X86_64 OpenBSD -> "openbsd64"
    Platform I386   Windows -> "windows32"
    Platform X86_64 Windows -> "windows64"
    Platform Arm    Linux   -> "linux-armv7"
    _ -> error $ "Unrecognized platform: " ++ Cabal.display buildPlatform

setupUrl :: String
setupUrl = "https://raw.githubusercontent.com/fpco/stackage-content/master/stack"

downloadAndInstallGHC
    :: Version -> Action InstalledGhc
downloadAndInstallGHC version = do
    setupYaml <- askDownload Download
                    { downloadFilePrefix = "stackage/setup"
                    , downloadName = "stack-setup-2.yaml"
                    , downloadUrlPrefix = setupUrl
                    }
    -- TODO: don't re-parse the yaml for every GHC version
    cs <- readArtifactB setupYaml
    download <- case decodeEither' cs of
        Left err -> throw err
        Right x
            | Just download <- HM.lookup version (ghcVersions x)
                -> pure download
            | otherwise -> fail $ "Couldn't find GHC version" ++ Cabal.display version
    -- TODO: reenable this once we've fixed the issue with nondetermistic
    -- temp file locations.
    -- rerunIfCleaned
    let (url, f) = splitFileName $ downloadUrl download
    tar <- askDownload Download
            { downloadFilePrefix = "stackage/ghc"
            , downloadName = f
            , downloadUrlPrefix = url
            }
    -- TODO: check file size and sha1
    -- GHC's configure step requires an absolute prefix.
    -- We'll install it explicitly in ${TMPDIR}, but that puts explicit references
    -- to those paths in the package DB.  So we'll then generate a new DB with
    -- relative paths.
    let installDir = "ghc-install"
    let unpackedDir = versionedGhc version
    installed <- runCommand
       (output installDir)
       $ message "Unpacking GHC"
          <> input tar
          <> prog "tar" ["-xJf", pathIn tar, "-C", pathOut ""]
          <> withCwd (pathOut unpackedDir)
                (message "Installing GHC locally"
                <> progTemp (unpackedDir </> "configure")
                        ["--prefix=${TMPDIR}/" ++ pathOut installDir]
                <> prog "make" ["install"])
    return InstalledGhc { ghcLibRoot = installed /> "lib" </> versionedGhc version
                        , ghcInstalledVersion = version
                        }

versionedGhc :: Version -> String
versionedGhc version = "ghc-" ++ Cabal.display version

makeRelativeGlobalDb :: [PackageName] -> InstalledGhc -> Action InstalledGhc
makeRelativeGlobalDb corePkgs ghc = do
    let corePkgsSet = Set.fromList corePkgs
    -- List all packages, excluding Cabal which stack doesn't consider a "core"
    -- package.
    -- TODO: if our package ids included a hash, this wouldn't be as big a problem
    -- because two versions of the same package could exist simultaneously.
    builtinPackages <- fmap (filter ((`Set.member` corePkgsSet) . mkPackageName)
                                . words)
                        . runCommandStdout
                        $ ghcPkgProg ghc
                            ["list", "--global", "--names-only",
                             "--simple-output" ]
    let makePkgConf pkg = do
            desc <- runCommandStdout
                        $ ghcPkgProg ghc ["describe", pkg]
            let tempRoot = parsePkgRoot desc
            let desc' =
                    T.unpack
                    . T.replace (T.pack tempRoot)
                        (T.pack "${pkgroot}")
                    . T.pack
                    $ desc
            writeArtifact (pkg ++ ".conf") desc'
    confs <- mapM makePkgConf builtinPackages
    -- let globalRelativePackageDb = "global-packages/package-fixed.conf.d"
    let ghcFixed = "ghc-fixed"
    let db = pathOut (ghcFixed </> packageConfD)
    let ghcPkg = progTemp (ghcFixed </> "bin/ghc-pkg")
    ghcDir <- runCommand (output ghcFixed)
                $ shadow (ghcLibRoot ghc) ghcFixed
                <> inputList confs
                <> message "Making global DB relative"
                <> prog "rm" ["-rf", db]
                <> ghcPkg ["init", db]
                <> foldMap
                        (\conf -> ghcPkg
                            [ "register", pathIn conf
                            , "--global-package-db=" ++ db
                            , "--no-user-package-db"
                            , "--no-user-package-conf"
                            , "--no-expand-pkgroot"
                            , "--force"
                            ])
                        confs
    return ghc { ghcLibRoot = ghcDir }

-- TODO: this gets the TMPDIR that was used when installing; consider allowing
-- that to be captured explicitly.
parsePkgRoot :: String -> String
parsePkgRoot desc = loop $ lines desc
  where
    loop [] = error "Couldn't parse pkgRoot: " ++ show desc
    loop (l:ls)
        | take (length prefix) l == prefix = takeDirectory
                                            $ drop (length prefix) l
        | otherwise = loop ls
    prefix = "library-dirs: "
