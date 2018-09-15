{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pier.Core.Internal.HashableSet
    ( HashableSet(..)
    ) where

import qualified Data.Set as Set
import Development.Shake.Classes

-- | A newtype wrapper for 'Data.Set' which is an instance of 'Hashable',
-- so it can be used in Shake rules.
newtype HashableSet a = HashableSet { unHashableSet :: Set.Set a }
    deriving (Eq, Binary, NFData, Semigroup, Monoid)

instance Hashable a => Hashable (HashableSet a) where
    hashWithSalt k = hashWithSalt k . Set.toList . unHashableSet
