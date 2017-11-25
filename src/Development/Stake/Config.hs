{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Development.Stake.Config where

import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Yaml
import Development.Stake.Stackage
import Development.Shake
import Development.Shake.Classes
import Distribution.Package
import Distribution.Version
import GHC.Generics hiding (packageName)

-- TODO: rename; maybe ConfigSpec and ConfigEnv?  Or Config and Env?
data StackYaml = StackYaml
    { resolver :: PlanName
    , packages :: [FilePath]
    , extraDeps :: [PackageIdentifier]
    } deriving (Show, Eq, Typeable, Generic)
instance Hashable StackYaml
instance Binary StackYaml
instance NFData StackYaml

instance FromJSON StackYaml where
    parseJSON = withObject "StackYaml" $ \o -> do
        r <- o .: "resolver"
        pkgs <- o .: "packages"
        ed <- o .: "extra-deps"
        return StackYaml
            { resolver = r
            , packages = fromMaybe [] pkgs
            , extraDeps = fromMaybe [] ed
            }

data Config = Config
    { plan :: BuildPlan
    , configExtraDeps :: HM.HashMap PackageName Version
    , localPackages :: HM.HashMap PackageName FilePath
    , configGhc :: InstalledGhc
    } deriving Show

-- TODO: cache?
askConfig :: StackYaml -> Action Config
askConfig yaml = do
    p <- askBuildPlan (resolver yaml)
    ghc <- askInstalledGhc (ghcVersion p)
    return Config
        { plan = p
        , configGhc = ghc
        , localPackages = HM.empty
        , configExtraDeps = HM.fromList $ [ (packageName pkg, packageVersion pkg)
                                          | pkg <- extraDeps yaml
                                          ]
        }

data Resolved = Resolved Way PackageId
    deriving (Show,Typeable,Eq,Generic)
instance Hashable Resolved
instance Binary Resolved
instance NFData Resolved

data Way = Builtin | Additional
    deriving (Show,Typeable,Eq,Generic)
instance Hashable Way
instance Binary Way
instance NFData Way

resolvePackage :: Config -> PackageName -> Resolved
resolvePackage conf n
    -- TODO: nicer syntax
    -- core packages can't be overridden.  (TODO: is this right?)
    | Just v <- HM.lookup n (corePackageVersions $ plan conf)
                = Resolved Builtin $ PackageIdentifier n v
    -- Extra-deps override packages in the build plan:
    | Just v <- HM.lookup n (configExtraDeps conf)
                = Resolved Additional $ PackageIdentifier n v
    | Just v <- HM.lookup n (packageVersions $ plan conf)
                = Resolved Additional $ PackageIdentifier n v
    | otherwise = error $ "Couldn't find package " ++ show n
