{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Development.Pier.Config
    ( configRules
    , askConfig
    , Config(..)
    , Resolved(..)
    , resolvePackage
    , resolvedPackageId
    ) where

import Control.Exception (throw)
import Control.Monad (void)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Yaml
import Development.Pier.Stackage
import Development.Pier.Command
import Development.Pier.Package
import Development.Pier.Persistent
import Distribution.Package
import Distribution.Version
import Development.Shake
import Development.Shake.Classes

import GHC.Generics hiding (packageName)

data StackYamlPath = StackYamlPath
    deriving (Show, Eq, Typeable, Generic)
instance Hashable StackYamlPath
instance Binary StackYamlPath
instance NFData StackYamlPath

type instance RuleResult StackYamlPath = FilePath

configRules :: FilePath -> Rules ()
configRules f = do
    void $ addOracle $ \StackYamlPath -> return f
    void $ addPersistent $ \StackYamlQ -> do
        path <- askOracle StackYamlPath
        need [path]
        yamlE <- liftIO $ decodeFileEither path
        either (liftIO . throw) return yamlE

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
        pkgs <- o .:? "packages"
        ed <- o .:? "extra-deps"
        return StackYaml
            { resolver = r
            , packages = fromMaybe [] pkgs
            , extraDeps = fromMaybe [] ed
            }

data StackYamlQ = StackYamlQ
    deriving (Show, Eq, Typeable, Generic)
instance Hashable StackYamlQ
instance Binary StackYamlQ
instance NFData StackYamlQ

type instance RuleResult StackYamlQ = StackYaml


data Config = Config
    { plan :: BuildPlan
    , configExtraDeps :: HM.HashMap PackageName Version
    , localPackages :: HM.HashMap PackageName (Artifact, Version)
    , configGhc :: InstalledGhc
    } deriving Show

-- TODO: cache?
askConfig :: Action Config
askConfig = do
    yaml <- askPersistent StackYamlQ
    p <- askBuildPlan (resolver yaml)
    ghc <- askInstalledGhc (ghcVersion p)
    -- TODO: don't parse local package defs twice.
    -- We do it again later so the full PackageDescription
    -- doesn't need to get saved in the cache.
    pkgDescs <- mapM (\f -> do
                        let a = externalFile f
                        pkg <- parseCabalFileInDir a
                        return (packageName pkg, (a, packageVersion pkg)))
                    $ packages yaml
    return Config
        { plan = p
        , configGhc = ghc
        , localPackages = HM.fromList pkgDescs
        , configExtraDeps = HM.fromList [ (packageName pkg, packageVersion pkg)
                                        | pkg <- extraDeps yaml
                                        ]
        }

data Resolved
    = Builtin PackageId
    | Hackage PackageId
    | Local Artifact PackageId
    deriving (Show,Typeable,Eq,Generic)
instance Hashable Resolved
instance Binary Resolved
instance NFData Resolved

resolvePackage :: Config -> PackageName -> Resolved
resolvePackage conf n
    -- TODO: nicer syntax
    -- core packages can't be overridden.  (TODO: is this right?)
    | Just v <- HM.lookup n (corePackageVersions $ plan conf)
                = Builtin $ PackageIdentifier n v
    | Just (a, v) <- HM.lookup n (localPackages conf)
                = Local a $ PackageIdentifier n v
    -- Extra-deps override packages in the build plan:
    | Just v <- HM.lookup n (configExtraDeps conf)
                = Hackage $ PackageIdentifier n v
    | Just v <- HM.lookup n (packageVersions $ plan conf)
                = Hackage $ PackageIdentifier n v
    | otherwise = error $ "Couldn't find package " ++ show n


resolvedPackageId :: Resolved -> PackageIdentifier
resolvedPackageId (Builtin p) = p
resolvedPackageId (Hackage p) = p
resolvedPackageId (Local _ p) = p
