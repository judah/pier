{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Development.Stake.Config where

import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Yaml
import Development.Stake.Stackage
import Development.Shake
import Development.Shake.Classes
import Development.Stake.Command
import Distribution.Package
import Distribution.PackageDescription.Parse
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
    , localPackages :: HM.HashMap PackageName Artifact
    , configGhc :: InstalledGhc
    } deriving Show

-- TODO: cache?
askConfig :: StackYaml -> Action Config
askConfig yaml = do
    p <- askBuildPlan (resolver yaml)
    ghc <- askInstalledGhc (ghcVersion p)
    -- TODO: don't parse local package defs twice.
    -- We do it again later so the full PackageDescription
    -- doesn't need to get saved in the cache.
    pkgDescs <- mapM (\f -> do
                                let a = externalFile f
                                n <- getPackageNameFromDir a
                                return (n, a))
                    $ packages yaml
    return Config
        { plan = p
        , configGhc = ghc
        , localPackages = HM.fromList pkgDescs
        , configExtraDeps = HM.fromList $ [ (packageName pkg, packageVersion pkg)
                                          | pkg <- extraDeps yaml
                                          ]
        }

-- TODO: merge with Development.Stake.Package.
-- Not sure if the semantics around searching for *.cabal files
-- can be the same.
getPackageNameFromDir :: Artifact -> Action PackageName
getPackageNameFromDir a = do
    cabalFiles <- matchArtifactGlob a "*.cabal"
    case cabalFiles of
        [f] -> do
                cabalContents <- readArtifact f
                case parsePackageDescription cabalContents of
                    ParseFailed err -> error $ show err
                    ParseOk _ pkg -> return $ packageName pkg
        [] -> error $ "No *.cabal files found in " ++ show a
        _ -> error $ "Multiple *.cabal files found: " ++ show cabalFiles

data Resolved
    = Builtin PackageId
    | Hackage PackageId
    | Local PackageName Artifact
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
    | Just a <- HM.lookup n (localPackages conf)
                = Local n a
    -- Extra-deps override packages in the build plan:
    | Just v <- HM.lookup n (configExtraDeps conf)
                = Hackage $ PackageIdentifier n v
    | Just v <- HM.lookup n (packageVersions $ plan conf)
                = Hackage $ PackageIdentifier n v
    | otherwise = error $ "Couldn't find package " ++ show n
