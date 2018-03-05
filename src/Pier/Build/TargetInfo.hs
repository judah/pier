module Pier.Build.TargetInfo
    ( TargetInfo(..)
    , TargetResult(..)
    , TransitiveDeps(..)
    , getTargetInfo
    ) where

import Control.Monad (filterM)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe)
import Development.Shake
import Development.Shake.FilePath ((</>))
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.ModuleName
import Distribution.Package (packageName)
import Distribution.PackageDescription
import Distribution.Text (display)
import Language.Haskell.Extension

import Pier.Build.CFlags
import Pier.Build.ConfiguredPackage
import Pier.Build.Module
import Pier.Build.Stackage
import Pier.Core.Artifact

data TargetInfo = TargetInfo
    { targetCFlags :: CFlags
    , targetSourceInputs :: [Artifact]
        -- ^ Source files to pass on command line
    , targetOtherInputs :: [Artifact]
        -- ^ Other files to pass on command line
    , targetOptions :: [String]
    , targetIncludeDirs :: [FilePath]
        -- ^ Directories in which GHC should look for includes
        -- TODO: merge with CFlags?  Make Artifact?
    , targetSourceDirs :: [Artifact]
        -- ^ Directories in which GHC should look for boot files
    , targetOtherModules :: [ModuleName]
    }

data TargetResult
    = TargetBinary { targetModulePath :: FilePath }
    | TargetLibrary
        { targetExposedModules :: [ModuleName]
        }

getTargetInfo ::
       ConfiguredPackage
    -> BuildInfo
    -> TargetResult
    -> TransitiveDeps
    -> InstalledGhc
    -> Action TargetInfo
getTargetInfo confd bi result deps ghc = do
    let packageSourceDir = confdSourceDir confd
    cflags <- getCFlags deps packageSourceDir bi
    let allOptions = map ("-X" ++)
                    (display (fromMaybe Haskell98 $ defaultLanguage bi)
                        : map display (defaultExtensions bi ++ oldExtensions bi))
                    ++ concat [opts | (GHC,opts) <- options bi]
    let srcDirs = sourceDirArtifacts packageSourceDir bi
    let fixDashes = map $ \c -> if c == '-' then '_' else c
    let pathsMod = fromString $ "Paths_" ++ fixDashes (display $ packageName confd)
    let allModules = otherModules bi ++ case result of
                        TargetLibrary exposed -> exposed
                        TargetBinary _
                            -- Add the Paths_ module automatically to other-modules
                            -- of binaries.
                            -- TODO: consider whether this is intended behavior of Cabal.
                            -> [pathsMod | pathsMod `notElem` otherModules bi]
    moduleFiles <- mapM (findModule ghc confd cflags srcDirs)
                            allModules
    moduleBootFiles <- catMaybes <$> mapM findBootFile moduleFiles
    let cFiles = map (packageSourceDir />) $ cSources bi
    cIncludes <- collectCIncludes (confdDesc confd) bi (packageSourceDir />)
    moduleMainFiles <- case result of
                        TargetLibrary{} -> return []
                        TargetBinary f -> do
                            path <- findMainFile ghc cflags srcDirs f
                            return [path | path `notElem` moduleFiles]
    return TargetInfo
        { targetCFlags = cflags
        , targetSourceInputs = cFiles ++ moduleFiles ++ moduleMainFiles
        , targetOtherInputs = cIncludes ++ moduleBootFiles
        , targetOptions = allOptions
        , targetIncludeDirs = includeDirs bi
        , targetSourceDirs = srcDirs
        , targetOtherModules = otherModules bi
        }
                            

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
