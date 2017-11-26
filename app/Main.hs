{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

import Control.Exception (throw)
import Data.Monoid ((<>))
import qualified Data.Yaml as Yaml
import Development.Shake hiding (command)
import Development.Stake.Build
import Development.Stake.Core
import Development.Stake.Command
import Development.Stake.Download
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
buildCommand = Build <$> stackYamlFlag <*> packageNames
  where
    stackYamlFlag = strOption (long "stack-yaml" <> metavar "YAML"
                                <> value "stack.yaml")
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
runWithOptions CleanAll =  liftIO unfreezeArtifacts >> cleanAll
runWithOptions (Build yamlPath pkgs)
    = action $ do
        yamlE <- liftIO $ Yaml.decodeFileEither yamlPath
        yaml <- either (liftIO . throw) return yamlE
        askBuiltPackages yaml pkgs

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
