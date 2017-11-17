{-# OPTIONS_GHC -Wno-orphans #-}
module Development.Stake.Orphans where

import Development.Shake.Classes
import qualified Data.Set as Set
import Distribution.PackageDescription
import Distribution.Package

instance Hashable a => Hashable (Set.Set a) where
    hashWithSalt k = hashWithSalt k . Set.toList

instance Hashable FlagName
instance Hashable PackageId
instance Hashable PackageName

