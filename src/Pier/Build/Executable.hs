{-# LANGUAGE DeriveAnyClass #-}
module Pier.Build.Executable
    ( askBuiltExecutable
    , BuiltExecutableQ(..)
    , BuiltBinary(..)
    , progBinary
    ) where

import Data.Set (Set)
import Development.Shake
import Development.Shake.Classes
import Distribution.Package (PackageName)
import Distribution.Text (display)
import GHC.Generics (Generic(..))

import Pier.Core.Artifact
import Pier.Core.Persistent

data BuiltBinary = BuiltBinary
    { builtBinary :: Artifact
    , builtBinaryDataFiles :: Set Artifact
    } deriving (Show, Eq, Generic, Hashable, Binary, NFData)


data BuiltExecutableQ = BuiltExecutableQ PackageName String
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)
type instance RuleResult BuiltExecutableQ = BuiltBinary

instance Show BuiltExecutableQ where
    show (BuiltExecutableQ p e) = "Executable " ++ e ++ " from " ++ display p

askBuiltExecutable :: PackageName -> String -> Action BuiltBinary
askBuiltExecutable p e = askPersistent $ BuiltExecutableQ p e

progBinary :: BuiltBinary -> [String] -> Command
progBinary exe args = progA (builtBinary exe) args
                <> inputs (builtBinaryDataFiles exe)
