{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Development.Stake.Stackage
    ( buildPlanRules
    , askBuildPlan
    , PlanName(..)
    , BuildPlan(..)
    , resolvePackage
    , Resolved(..)
    , Way(..)
    ) where

import GHC.Generics
import Data.Binary.Orphans ()
import Control.Exception (throw)
import Control.Monad ((>=>))
import qualified Data.HashMap.Strict as HM
import Data.List (isPrefixOf)
import Data.Text (Text, pack)
import Data.Yaml
import Distribution.Version
import qualified Distribution.Text as Cabal
import Distribution.Package
import Development.Shake.Classes hiding (get)
import Development.Stake.Command
import Development.Stake.Download
import Development.Stake.Orphans ()
import Development.Stake.Witness
import Development.Shake.FilePath
import Development.Shake

newtype PlanName = PlanName { renderPlanName :: String }
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)

data BuildPlan = BuildPlan
    { corePackageVersions :: HM.HashMap Text Version
    , packageVersions :: HM.HashMap Text Version
    , ghcVersion :: Version
    } deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)

instance FromJSON BuildPlan where
    parseJSON = withObject "Plan" $ \o -> do
        sys <- o .: "system-info"
        coreVersions <- (sys .: "core-packages") >>= mapM parseVersion
        ghcVers <- sys .: "ghc-version" >>= parseVersion
        pkgs <- o .: "packages"
        pkgVersions <- mapM ((.: "version") >=> parseVersion) pkgs
        return BuildPlan { corePackageVersions = coreVersions
                         , packageVersions = pkgVersions
                         , ghcVersion = ghcVers
        }

parseVersion :: String -> Parser Version
parseVersion s
    | Just v <- Cabal.simpleParse s = pure v
    | otherwise = fail $ "Unable to parse Version: " ++ show s

data Resolved = Resolved Way PackageId
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)

data Way = Builtin | Additional
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)

resolvePackage :: BuildPlan -> PackageName -> Resolved
resolvePackage bp n
    | Just v <- HM.lookup n' (corePackageVersions bp)
                = Resolved Builtin $ PackageIdentifier n v
    | Just v <- HM.lookup n' (packageVersions bp)
                = Resolved Additional $ PackageIdentifier n v
    | otherwise = error $ "Couldn't find package " ++ show n'

  where
    n' = pack $ unPackageName n

buildPlanRules :: Rules ()
buildPlanRules = do
    addWitness $ \(ReadPlan planName) -> do
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
    | "lts-" `isPrefixOf` name = ltsBuildPlansUrl
    | "nightly-" `isPrefixOf` name = nightlyBuildPlansUrl
    | otherwise = error $ "Unrecognized plan name " ++ show name
  where
    ltsBuildPlansUrl = "https://raw.githubusercontent.com/fpco/lts-haskell/master/"
    nightlyBuildPlansUrl = "https://raw.githubusercontent.com/fpco/stackage-nightly/master/"

newtype ReadPlan = ReadPlan PlanName
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)
type instance RuleResult ReadPlan = BuildPlan

askBuildPlan :: PlanName -> Action BuildPlan
askBuildPlan = askWitness . ReadPlan
