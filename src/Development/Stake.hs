{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake (main) where

import Data.Monoid ((<>))
import Development.Shake hiding (command)
import Development.Stake.Build
import Development.Stake.Core
import Development.Stake.Command
import Development.Stake.Download
import Development.Stake.Stackage
import Distribution.Package
import Options.Applicative hiding (action)
import System.Environment

data CommandOpt = Clean | CleanAll | Build PlanName [PackageName]
type ShakeFlag = String

verbosity :: Parser [ShakeFlag]
verbosity = fmap mk $ many $ flag' 'V' ( long "verbose"
                                         <> short 'V')
  where
    mk [] = []
    mk vs = ['-':vs]

parallelism :: Parser ShakeFlag
parallelism = fmap ("--jobs=" ++) $ strOption ( long "jobs"
                                            <> short 'j')

shakeArg :: Parser ShakeFlag
shakeArg = strOption (long "shake-arg" <> metavar "SHAKEARG")

shakeFlags :: Parser [ShakeFlag]
shakeFlags = mconcat <$> sequenceA
                            [ verbosity
                            , many parallelism
                            , many shakeArg
                            ]

cleanCommand :: Parser CommandOpt
cleanCommand = pure Clean

cleanAllCommand :: Parser CommandOpt
cleanAllCommand = pure CleanAll

buildCommand :: Parser CommandOpt
buildCommand = Build <$> planName <*> packageNames
  where
    planName = fmap PlanName $ strOption ( long "plan"
                                        <> short 'p'
                                        <> metavar "PLANNAME" )
    packageNames = (fmap . fmap) PackageName $ many
                                             $ strArgument ( metavar "PACKAGENAME" )

stakeCmd :: Parser CommandOpt
stakeCmd = subparser $
    command "clean" (info cleanCommand  (progDesc "Clean project")) <>
    command "clean-all" (info cleanAllCommand (progDesc "Clean project & dependencies")) <>
    command "build" (info buildCommand (progDesc "Build Project"))

opts :: ParserInfo (CommandOpt, [ShakeFlag])
opts = info args mempty
  where
    args = (,) <$> stakeCmd <*> shakeFlags

runWithOptions :: CommandOpt -> Rules ()
runWithOptions Clean = cleanBuild
runWithOptions CleanAll = cleanAll
runWithOptions (Build plan packages)
    = action $ askBuiltPackages plan packages

main :: IO ()
main = do
    (cmdOpt, flags) <- execParser opts
    withArgs flags $ runStake $ do
        buildPlanRules
        buildPackageRules
        commandRules
        downloadRules
        installGhcRules
        runWithOptions cmdOpt
