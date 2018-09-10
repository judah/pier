module Pier.Build.ConfiguredPackage
    ( ConfiguredPackage(..)
    , getConfiguredPackage
    , targetDepNames
    , allDependencies
    ) where

import Data.List (nub)
import Development.Shake
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Text (display)

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

import Pier.Build.Config
import Pier.Build.Custom
import Pier.Build.Package
import Pier.Build.Stackage (Flags, InstalledGhc)
import Pier.Core.Artifact

data ConfiguredPackage = ConfiguredPackage
    { confdDesc :: PackageDescription
    , confdSourceDir :: Artifact
    , confdDataFiles :: Maybe Artifact
    , confdExtraSrcFiles  :: [FilePath] -- relative to source dir
    }

instance Package ConfiguredPackage where
    packageId = packageId . confdDesc

-- TODO: merge with Resolved
-- TODO: don't copy everything if configuring a local package?  Or at least
-- treat deps less coarsely?
getConfiguredPackage
    :: PackageName -> Action (Either PackageId ConfiguredPackage)
getConfiguredPackage p = do
    conf <- askConfig
    case resolvePackage conf p of
        Builtin pid -> return $ Left pid
        Hackage pid flags -> do
            dir <- getPackageSourceDir pid
            Right . addHappyAlexSourceDirs <$> getConfigured conf flags dir
        Local dir _ -> Right <$> getConfigured conf HM.empty dir
  where
    getConfigured :: Config -> Flags -> Artifact -> Action ConfiguredPackage
    getConfigured conf flags dir = do
        (desc, dir') <- configurePackage (plan conf) flags dir
        datas <- collectDataFiles (configGhc conf) desc dir'
        extras <- fmap (nub . concat)
                            . mapM (matchArtifactGlob dir')
                            . extraSrcFiles
                            $ desc
        return $ ConfiguredPackage desc dir' datas extras

targetDepNames :: BuildInfo -> [PackageName]
targetDepNames bi = [n | Dependency n _ <- targetBuildDepends bi]

allDependencies :: PackageDescription -> [PackageName]
allDependencies desc = let
    allBis = [libBuildInfo l | Just l <- [library desc]]
                    ++ map buildInfo (executables desc)
                    ++ map testBuildInfo (testSuites desc)
                    ++ map benchmarkBuildInfo (benchmarks desc)
   in Set.toList . Set.fromList . concatMap targetDepNames $ allBis

addHappyAlexSourceDirs :: ConfiguredPackage -> ConfiguredPackage
addHappyAlexSourceDirs confd
    | packageName (confdDesc confd) `elem` map mkPackageName ["happy", "alex"]
        = confd { confdDesc = addDistSourceDirs $ confdDesc confd }
    | otherwise = confd

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
