{-# LANGUAGE DeriveAnyClass #-}
module Pier.Build.Executable
    ( askBuiltExecutable
    , BuiltExecutableQ(..)
    , BuiltExecutable(..)
    , progExe
    ) where

import Data.Semigroup
import Data.Set (Set)
import Development.Shake
import Development.Shake.Classes
import Distribution.Package (PackageName)
import Distribution.Text (display)
import GHC.Generics (Generic(..))

import Pier.Core.Artifact
import Pier.Core.Persistent

data BuiltExecutable = BuiltExecutable
    { builtBinary :: Artifact
    , builtExeDataFiles :: Set Artifact
    } deriving (Show, Eq, Generic, Hashable, Binary, NFData)


data BuiltExecutableQ = BuiltExecutableQ PackageName String
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)
type instance RuleResult BuiltExecutableQ = BuiltExecutable

instance Show BuiltExecutableQ where
    show (BuiltExecutableQ p e) = "Executable " ++ e ++ " from " ++ display p

askBuiltExecutable :: PackageName -> String -> Action BuiltExecutable
askBuiltExecutable p e = askPersistent $ BuiltExecutableQ p e

progExe :: BuiltExecutable -> [String] -> Command
progExe exe args = progA (builtBinary exe) args
                <> inputs (builtExeDataFiles exe)
