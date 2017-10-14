{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake (main) where

import Data.Monoid ((<>))
import Development.Shake hiding (command)
import Development.Stake.Build
import Development.Stake.Core
import Development.Stake.Package
import Development.Stake.Stackage
import Distribution.Package
import Options.Applicative


data Command = Clean | CleanAll | Build PlanName PackageName


cleanCommand :: Parser Command
cleanCommand = flag' undefined
  (  long "clean"
  <> short 'c'
  <> help "Clean project" )

cleanAllCommand :: Parser Command
cleanAllCommand = flag' undefined
  (  long "clean-all"
  <> help "Clean project & dependencies" )

buildCommand :: Parser Command
buildCommand = Build <$> planName <*> packageName
  where
    planName = PlanName <$> strOption
      (  long "build"
      <> short 'b'
      <> metavar "PLANNAME"
      <> help "Build project" )
    packageName = PackageName <$> strOption
      (  long "build"
      <> short 'f'
      <> metavar "PACKAGENAME"
      <> help "Build project" )


input :: Parser Command
input = cleanCommand  <|> cleanAllCommand <|> buildCommand

opts :: ParserInfo Command
opts = info input mempty

runWithOptions :: ParserResult Command -> Rules ()
runWithOptions = undefined


main :: IO ()
main = runStake $ \args -> do
    downloadCabalPackageRule
    buildPlanRules
    buildPackageRules
    runWithOptions $ execParserPure undefined opts args


