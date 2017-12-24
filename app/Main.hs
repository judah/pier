module Main (main) where

import Control.Monad (void)
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import Development.Shake hiding (command)
import Development.Stake.Build
import Development.Stake.Config
import Development.Stake.Core
import Development.Stake.Command
import Development.Stake.Download
import Development.Stake.Stackage
import Distribution.Package
import Distribution.Text (display, simpleParse)
import Options.Applicative hiding (action)
import System.Environment

data CommandOpt
    = Clean
    | CleanAll
    | Build [(PackageName, Target)]
    | Exec (PackageName, Target) [String]
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
buildCommand = Build <$> some (argument (eitherReader parseTarget) (metavar "TARGET"))

execCommand :: Parser CommandOpt
execCommand = Exec <$> argument (eitherReader parseTarget) (metavar "TARGET")
                <*> many (strArgument (metavar "ARGUMENT"))

stakeCmd :: Parser CommandOpt
stakeCmd = subparser $
    command "clean" (info cleanCommand  (progDesc "Clean project")) <>
    command "clean-all" (info cleanAllCommand (progDesc "Clean project & dependencies")) <>
    command "build" (info buildCommand (progDesc "Build Project")) <>
    command "exec" (info execCommand (progDesc "Run executable"))

opts :: ParserInfo (FilePath, CommandOpt, [ShakeFlag])
opts = info args mempty
  where
    args = (,,) <$> stackYamlFlag <*> stakeCmd <*> shakeFlags

    stackYamlFlag = strOption (long "stack-yaml" <> metavar "YAML"
                                <> value "stack.yaml")

runWithOptions :: CommandOpt -> Rules ()
runWithOptions Clean = cleanBuild
runWithOptions CleanAll =  liftIO unfreezeArtifacts >> cleanAll
runWithOptions (Build targets) = action $ forP targets (uncurry buildTarget)
runWithOptions (Exec (pkg, target) args) = action $ do
    name <- case target of
                TargetExe name -> return name
                TargetAll -> return $ display pkg
                TargetAllExes -> return $ display pkg
                TargetLib -> error "exec can't be used with a \"lib\" target"
    exe <- buildExecutableNamed pkg name
    liftIO $ callArtifact (builtExeDataFiles exe) (builtBinary exe) args

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

-- TODO: move into Build.hs
data Target
    = TargetAll
    | TargetLib
    | TargetAllExes
    | TargetExe String
    deriving Show

parseTarget :: String -> Either String (PackageName, Target)
parseTarget s = case splitOn ":" s of
    [n] -> (, TargetAll) <$> parsePackageName n
    [n, "lib"] -> (, TargetLib) <$> parsePackageName n
    [n, "exe"] -> (, TargetAllExes) <$> parsePackageName n
    [n, "exe", e] -> (, TargetExe e) <$> parsePackageName n
    _ -> Left $ "Error parsing target " ++ show s
  where
    parsePackageName n = case simpleParse n of
        Just p -> return p
        Nothing -> Left $ "Error parsing package name " ++ show n

buildTarget :: PackageName -> Target -> Action ()
buildTarget n TargetAll = void $ askMaybeBuiltLibrary n >> buildExecutables n
buildTarget n TargetLib = void $ askBuiltLibrary n
buildTarget n TargetAllExes = void $ buildExecutables n
buildTarget n (TargetExe e) = void $ buildExecutableNamed n e
