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
import qualified Data.ByteString as B
import Control.Exception (throw)
import Control.Lens ((^.))
import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HM
import Data.List (isPrefixOf)
import Data.String (IsString(..))
import Data.Text (Text, pack)
import Data.Yaml
import Distribution.Version
import qualified Distribution.Text as Cabal
import Network.Wreq
import Distribution.Package
import Development.Shake.Classes hiding (get)
import Development.Stake.Core
import Development.Stake.Orphans ()
import Development.Stake.Witness
import Development.Shake.FilePath
import Development.Shake

ltsBuildPlansUrl, nightlyBuildPlansUrl :: String
ltsBuildPlansUrl = "https://raw.githubusercontent.com/fpco/lts-haskell/master/"
nightlyBuildPlansUrl = "https://raw.githubusercontent.com/fpco/stackage-nightly/master/"

newtype PlanName = PlanName { renderPlanName :: String }
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)

instance IsString PlanName where
    -- TODO: better parse
    fromString s = let n = PlanName s
                    in planUrl n `seq` n

planUrl :: PlanName -> String
planUrl (PlanName name)
    | "lts-" `isPrefixOf` name = ltsBuildPlansUrl ++ name ++ ".yaml"
    | "nightly-" `isPrefixOf` name = nightlyBuildPlansUrl ++ name ++ ".yaml"
    | otherwise = error $ "Unrecognized plan name " ++ show name

downloadPlan :: PlanName -> FilePath -> Action ()
downloadPlan p path = do
    putNormal "Downloading plan..."
    r <- liftIO $ get $ planUrl p
    liftIO $ L.writeFile path $ r ^. responseBody

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
    "downloads/stackage/plan/*.yaml" #> \f [name] ->
        downloadPlan (PlanName name) f
    addWitness $ \(ReadPlan planName) -> do
        let f = artifact "downloads/stackage/plan"
                                </> renderPlanName planName <.> "yaml"
        need [f]
        liftIO $ decodeFileEither f >>= either throw return

newtype ReadPlan = ReadPlan PlanName
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)
type instance RuleResult ReadPlan = BuildPlan

askBuildPlan :: PlanName -> Action BuildPlan
askBuildPlan = askWitness . ReadPlan
