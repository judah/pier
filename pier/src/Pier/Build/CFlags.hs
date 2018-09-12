{-# LANGUAGE DeriveAnyClass #-}
module Pier.Build.CFlags
    ( TransitiveDeps(..)
    , CFlags(..)
    , getCFlags
    , ghcDefines
    ) where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Set (Set)
import Development.Shake
import Development.Shake.Classes
import Distribution.PackageDescription
import Distribution.System (buildOS, OS(OSX))
import Distribution.Text (display)
import Distribution.Types.PkgconfigDependency
import Distribution.Version (versionNumbers)
import GHC.Generics (Generic(..))

import qualified Data.Set as Set

import Pier.Build.Stackage
import Pier.Core.Artifact

data TransitiveDeps = TransitiveDeps
    { transitiveDBs :: Set Artifact
    , transitiveLibFiles :: Set Artifact
    , transitiveIncludeDirs :: Set Artifact
    , transitiveDataFiles :: Set Artifact
    } deriving (Show, Eq, Typeable, Generic, Hashable, Binary, NFData)

instance Semigroup TransitiveDeps where
    (<>) = mappend

instance Monoid TransitiveDeps where
    mempty = TransitiveDeps Set.empty Set.empty Set.empty Set.empty
    TransitiveDeps dbs files is datas
        `mappend` TransitiveDeps dbs' files' is' datas'
        = TransitiveDeps (dbs <> dbs') (files <> files') (is <> is')
                (datas <> datas')

-- TODO: macros file also
data CFlags = CFlags
    { ccFlags :: [String]
    , cppFlags :: [String]
    , cIncludeDirs :: Set Artifact
    , linkFlags :: [String]
    , linkLibs :: [String]
    , macFrameworks :: [String]
    }

-- TODO: include macros file too
getCFlags :: TransitiveDeps -> Artifact -> BuildInfo -> Action CFlags
getCFlags deps pkgDir bi = do
    pkgConfFlags <- mconcat <$> mapM getPkgConfFlags (pkgconfigDepends bi)
    return CFlags
            { ccFlags = ccOptions bi ++ fst pkgConfFlags
            , cppFlags = cppOptions bi
            , cIncludeDirs =
                    Set.fromList (map (pkgDir />) $ includeDirs bi)
                    <> transitiveIncludeDirs deps
            , linkFlags = ldOptions bi ++ snd pkgConfFlags
            , linkLibs = extraLibs bi
            , macFrameworks = guard (buildOS == OSX)
                                >> frameworks bi
            }

-- TODO: handle version numbers too
getPkgConfFlags :: PkgconfigDependency -> Action ([String], [String])
getPkgConfFlags (PkgconfigDependency name _) = liftA2 (,)
    (runPkgConfig [display name, "--cflags"])
    (runPkgConfig [display name, "--libs"])
  where
    runPkgConfig = fmap words . runCommandStdout . prog "pkg-config"


-- | Definitions that GHC provides by default
ghcDefines :: InstalledGhc -> [String]
ghcDefines ghc = ["-D__GLASGOW_HASKELL__=" ++
                    cppVersion (ghcInstalledVersion ghc)]
  where
    cppVersion v = case versionNumbers v of
        (v1:v2:_) -> show v1 ++ if v2 < 10 then '0':show v2 else show v2
        _ -> error $ "cppVersion: " ++ display v


