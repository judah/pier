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

data Command = Clean | CleanAll | Build PlanName [PackageName] 
type ShakeFlag = String

verbosity :: Parser ShakeFlag
verbosity = fmap ('-':) $ many (flag' 'V' ( long "verbose"
                                         <> short 'V'))

parallelism :: Parser ShakeFlag
parallelism = fmap ("-jobs=" ++) $ strOption ( long "jobs"
                                            <> short 'j' )

shakeArg :: Parser ShakeFlag
shakeArg = strOption ( long "shake-arg" <> metavar "SHAKEARG" )

shakeFlags :: Parser [ShakeFlag]
shakeFlags = (\a b c -> a : b : c) <$> verbosity <*> parallelism <*> (many shakeArg)

cleanCommand :: Parser Command
cleanCommand = pure Clean

cleanAllCommand :: Parser Command
cleanAllCommand = pure CleanAll

buildCommand :: Parser Command
buildCommand = Build <$> planName <*> packageNames
  where
    planName = fmap PlanName $ strOption ( long "plan"
                                        <> short 'p'
                                        <> metavar "PLANNAME" )
    packageNames = (fmap . fmap) PackageName $ many 
                                             $ strArgument ( metavar "PACKAGENAME" )

stakeCmd :: Parser Command
stakeCmd = subparser $
    command "clean" (info cleanCommand  (progDesc "Clean project")) <>
    command "clean-all" (info cleanAllCommand (progDesc "Clean project & dependencies")) <>
    command "build" (info buildCommand (progDesc "Build Project"))

opts :: ParserInfo (Command, [ShakeFlag])
opts = info input mempty
  where
    input = (,) <$> stakeCmd <*> shakeFlags

runWithOptions :: Command -> Rules ()
runWithOptions cmd = do
  case cmd of
    Clean -> cleanBuild
    CleanAll -> cleanAll
    Build plan packages -> action $ do
        askBuiltPackages plan packages

main :: IO ()
main = do
    (stakeCmd,shakeFlags) <- execParser opts
    withArgs shakeFlags $ runStake $ do
        downloadCabalPackageRule
        buildPlanRules
        buildPackageRules
        runWithOptions stakeCmd
