{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake (main) where

import Data.Monoid ((<>))
import Development.Shake hiding (command)
import Development.Stake.Build
import Development.Stake.Core
import Development.Stake.Package
import Development.Stake.Stackage
import Distribution.Package
import Options.Applicative hiding (action)
import System.Environment

data Command = Clean | CleanAll | Build PlanName PackageName

cleanCommand :: Parser Command
cleanCommand = pure Clean

cleanAllCommand :: Parser Command
cleanAllCommand = pure CleanAll

buildCommand :: Parser Command
buildCommand = Build <$> planName <*> packageName
  where
    planName = PlanName <$> strOption ( long "plan"
                                     <> short 'p'
                                     <> metavar "PLANNAME" )
    packageName = PackageName <$> strArgument ( metavar "PACKAGENAME" )
                
input :: Parser Command
input = subparser $
    command "clean" (info cleanCommand  (progDesc "Clean project")) <>
    command "clean-all" (info cleanAllCommand (progDesc "Clean project & dependencies")) <>
    command "build" (info buildCommand (progDesc "Build Project"))

opts :: ParserInfo Command
opts = info input mempty

runWithOptions :: Command -> Rules ()
runWithOptions cmd = do
  case cmd of
    Clean -> cleanBuild
    CleanAll -> cleanAll
    Build plan packages -> action $ do
        askBuiltPackages plan [packages]

main :: IO ()
main = do
    res <- execParser opts
    --pass no args to shake
    withArgs [] $ runStake $ do
        downloadCabalPackageRule
        buildPlanRules
        buildPackageRules
        runWithOptions res
