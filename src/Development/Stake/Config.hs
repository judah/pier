{-# LANGUAGE OverloadedStrings #-}
module Development.Stake.Config
    where

import Control.Exception (throw)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Yaml
import Development.Stake.Stackage

data Config = Config
    { configResolver :: PlanName
    , configPackages :: [FilePath]
    } deriving Show

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        r <- o .: "resolver"
        pkgs <- o .: "packages"
        return Config { configResolver = PlanName r, configPackages = pkgs }

-- TODO: don't bother parsing config when it hasn't changed
readConfig :: IO Config
readConfig = decodeFileEither "stack.yaml" >>= either throw return

hashConfig :: Config -> Hash.Digest Hash.SHA256
hashConfig = Hash.hash . E.encodeUtf8 . T.pack . show
