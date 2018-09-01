{-# LANGUAGE DeriveAnyClass #-}
module Pier.Build.TestSuite
    ( askBuiltTestSuite
    , BuiltTestSuiteQ(..)
    , BuiltTestSuite(..)
    , progTestSuite
    ) where

import Data.Set (Set)
import Development.Shake
import Development.Shake.Classes
import Distribution.Package (PackageName)
import Distribution.Text (display)
import GHC.Generics (Generic(..))

import Pier.Core.Artifact
import Pier.Core.Persistent

data BuiltTestSuite = BuiltTestSuite
    { builtTestSuiteBinary :: Artifact
    , builtTestSuiteDataFiles :: Set Artifact
    } deriving (Show, Eq, Generic, Hashable, Binary, NFData)


data BuiltTestSuiteQ = BuiltTestSuiteQ PackageName String
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)
type instance RuleResult BuiltTestSuiteQ = BuiltTestSuite

instance Show BuiltTestSuiteQ where
    show (BuiltTestSuiteQ p s) = "TestSuite " ++ s ++ " from " ++ display p

askBuiltTestSuite :: PackageName -> String -> Action BuiltTestSuite
askBuiltTestSuite p e = askPersistent $ BuiltTestSuiteQ p e

progTestSuite :: BuiltTestSuite -> [String] -> Command
progTestSuite suite args = progA (builtTestSuiteBinary suite) args
                <> inputs (builtTestSuiteDataFiles suite)
