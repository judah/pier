module Pier.Build.ConfiguredPackage
    ( ConfiguredPackage(..)
    , getConfiguredPackage
    , targetDepNames
    , allDependencies
    ) where

import Development.Shake
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Build.Macros (generatePackageVersionMacros)

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

import Pier.Build.Config
import Pier.Build.Custom (addDistSourceDirs)
import Pier.Build.Package
import Pier.Build.Stackage (Flags)
import Pier.Core.Artifact

data ConfiguredPackage = ConfiguredPackage
    { confdDesc :: PackageDescription
    , confdSourceDir :: Artifact
    , confdMacros :: Artifact
        -- ^ Provides Cabal macros like VERSION_*
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
        macros <- genCabalMacros conf desc
        return $ ConfiguredPackage desc dir' macros

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

addHappyAlexSourceDirs :: ConfiguredPackage -> ConfiguredPackage
addHappyAlexSourceDirs confd
    | packageName (confdDesc confd) `elem` map mkPackageName ["happy", "alex"]
        = confd { confdDesc = addDistSourceDirs $ confdDesc confd }
    | otherwise = confd
