{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake (main) where

import Data.Monoid ((<>))
import Development.Shake hiding (command)
import Development.Stake.Build
import Development.Stake.Core
import Development.Stake.Config
import Development.Stake.Command
import Development.Stake.Package
import Development.Stake.Stackage
import Distribution.Package
import Options.Applicative hiding (action)
import System.Environment

data CommandOpt = Clean | CleanAll | Build FilePath [PackageName]
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
buildCommand = Build <$> stackYaml <*> packageNames
  where
    packageNames = (fmap . fmap) PackageName $ many
                                             $ strArgument ( metavar "PACKAGENAME" )
    stackYaml = strOption (long "stack-yaml" <> metavar "PATH"
                            <> value "stack.yaml" )

stakeCmd :: Parser CommandOpt
stakeCmd = subparser $
    command "clean" (info cleanCommand  (progDesc "Clean project")) <>
    command "clean-all" (info cleanAllCommand (progDesc "Clean project & dependencies")) <>
    command "build" (info buildCommand (progDesc "Build Project"))

opts :: ParserInfo (CommandOpt, [ShakeFlag])
opts = info input mempty
  where
    input = (,) <$> stakeCmd <*> shakeFlags

runWithOptions :: CommandOpt -> Rules ()
runWithOptions Clean = cleanBuild
runWithOptions CleanAll = cleanAll
runWithOptions (Build stackYaml packages) = do
    config <- liftIO $ readConfig stackYaml
    action $ askBuiltPackages (configResolver config) packages

main :: IO ()
main = do
    (cmdOpt, flags) <- execParser opts
    withArgs flags $ runStake $ do
        downloadCabalPackageRule
        buildPlanRules
        buildPackageRules
        runWithOptions cmdOpt
        commandRules
