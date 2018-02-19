-- stack script --resolver lts-10.3 --package pier --package unordered-containers
module Main (main) where

import Control.Exception (throw)
import qualified Data.HashMap.Strict as HM
import Data.Yaml
import Distribution.Text (display)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Pier.Build.Stackage

main = do
    [path] <- getArgs
    decoded <- decodeFileEither path
    case decoded of
        Left e -> throw e
        Right plan ->
            putStrLn . unlines . map display . HM.keys $ planPackages plan
