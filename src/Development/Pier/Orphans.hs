{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Development.Pier.Orphans () where

import Data.Aeson.Types
import qualified Data.Text as T
import Development.Shake.Classes
import qualified Data.Set as Set
import Distribution.PackageDescription
import Distribution.Package
import qualified Distribution.Text as Cabal

instance Hashable a => Hashable (Set.Set a) where
    hashWithSalt k = hashWithSalt k . Set.toList

instance Hashable FlagName
instance Hashable PackageId
instance Hashable PackageName
instance Hashable ComponentId
instance Hashable UnitId

instance FromJSON PackageIdentifier where
    parseJSON = withText "PackageIdentifier" simpleParser

deriving instance FromJSONKey PackageName

simpleParser :: Cabal.Text a => T.Text -> Parser a
simpleParser t = case Cabal.simpleParse (T.unpack t) of
                        Just v -> pure v
                        Nothing -> fail $ "Unable to parse PackageIdentifier: "
                                            ++ show t

