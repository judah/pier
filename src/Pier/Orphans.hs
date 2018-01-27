-- | All-purpose module for defining orphan instances.
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Pier.Orphans () where

import Data.Aeson.Types
import qualified Data.Text as T
import Development.Shake.Classes
import qualified Data.Set as Set
import Distribution.PackageDescription
import Distribution.Package
import qualified Distribution.Text as Cabal

import Distribution.Version
import Distribution.Utils.ShortText

instance Hashable a => Hashable (Set.Set a) where
    hashWithSalt k = hashWithSalt k . Set.toList

instance Hashable FlagName
instance NFData FlagName
instance Hashable PackageId
instance Hashable PackageName
instance Hashable ComponentId
instance Hashable UnitId
instance Hashable ShortText
instance Hashable Version

instance FromJSON Version where
    parseJSON = withText "Version" simpleParser

instance FromJSONKey Version where
    fromJSONKey = cabalKeyTextParser

instance FromJSON PackageName where
    parseJSON = withText "PackageName" simpleParser

instance FromJSONKey PackageName where
    fromJSONKey = cabalKeyTextParser

instance FromJSON FlagName where
    parseJSON = fmap mkFlagName . parseJSON

instance FromJSONKey FlagName where
    fromJSONKey = FromJSONKeyText (mkFlagName . T.unpack)

instance FromJSON PackageIdentifier where
    parseJSON = withText "PackageIdentifier" simpleParser

simpleParser :: Cabal.Text a => T.Text -> Parser a
simpleParser t = case Cabal.simpleParse (T.unpack t) of
                        Just v -> pure v
                        Nothing -> fail $ "Unable to parse PackageIdentifier: "
                                            ++ show t

cabalKeyTextParser :: Cabal.Text a => FromJSONKeyFunction a
cabalKeyTextParser = FromJSONKeyTextParser simpleParser
