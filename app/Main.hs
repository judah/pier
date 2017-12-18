module Main (main) where

import Data.Monoid ((<>))
import Development.Shake hiding (command)
import Development.Stake.Build
import Development.Stake.Config
import Development.Stake.Core
import Development.Stake.Command
import Development.Stake.Download
import Development.Stake.Stackage
import Distribution.Package
import Options.Applicative hiding (action)
import System.Environment

data CommandOpt = Clean | CleanAll | Build [PackageName]
type ShakeFlag = String

verbosity :: Parser [ShakeFlag]
verbosity = fmap mk $ many $ flag' 'V' ( long "verbose"
                                         <> short 'V')
  where
    mk [] = []
    mk vs = ['-':vs]

parallelism :: Parser ShakeFlag
parallelism = ("--jobs=" ++) <$> strOption ( long "jobs"
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
buildCommand = Build <$> packageNames
  where
    packageNames = (fmap . fmap) PackageName $ many
                                             $ strArgument ( metavar "PACKAGENAME" )

stakeCmd :: Parser CommandOpt
stakeCmd = subparser $
    command "clean" (info cleanCommand  (progDesc "Clean project")) <>
    command "clean-all" (info cleanAllCommand (progDesc "Clean project & dependencies")) <>
    command "build" (info buildCommand (progDesc "Build Project"))

opts :: ParserInfo (FilePath, CommandOpt, [ShakeFlag])
opts = info args mempty
  where
    args = (,,) <$> stackYamlFlag <*> stakeCmd <*> shakeFlags

    stackYamlFlag = strOption (long "stack-yaml" <> metavar "YAML"
                                <> value "stack.yaml")

runWithOptions :: CommandOpt -> Rules ()
runWithOptions Clean = cleanBuild
runWithOptions CleanAll =  liftIO unfreezeArtifacts >> cleanAll
runWithOptions (Build pkgs)
    = action $ askBuiltPackages pkgs

main :: IO ()
main = do
    (stackYamlPath, cmdOpt, flags) <- execParser opts
    withArgs flags $ runStake $ do
        buildPlanRules
        buildPackageRules
        commandRules
        downloadRules
        installGhcRules
        configRules stackYamlPath
        runWithOptions cmdOpt
