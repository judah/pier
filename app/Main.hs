{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import Control.Monad (void)
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import Development.Pier.Config
import Development.Pier.Core
import Development.Pier.Command
import Development.Pier.Download
import Development.Pier.Persistent
import Development.Pier.Stackage
import Development.Pier.Build
import Development.Shake hiding (command)
import Development.Shake.FilePath ((</>), takeDirectory, splitFileName)
import Distribution.Package
import Distribution.Text (display, simpleParse)
import Options.Applicative hiding (action)
import System.Directory as Directory
import System.Environment

data CommandOpt
    = Clean
    | CleanAll
    | Build [(PackageName, Target)]
    | Exec (PackageName, Target) [String]
    | Which (PackageName, Target)
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
buildCommand = Build <$> some parseTarget

execCommand :: Parser CommandOpt
execCommand = Exec <$> parseTarget <*> many (strArgument (metavar "ARGUMENT"))

whichCommand :: Parser CommandOpt
whichCommand = Which <$> parseTarget


pierCmd :: Parser CommandOpt
pierCmd = subparser $ mconcat
    [ command "clean" (cleanCommand `info` progDesc "Clean project")
    , command "clean-all" (cleanAllCommand `info` progDesc "Clean project & dependencies")
    , command "build" (buildCommand `info` progDesc "Build Project")
    , command "exec" (execCommand `info` progDesc "Run executable")
    , command "which" (whichCommand `info` progDesc
                            "Build executable and print its location")
    ]

opts :: ParserInfo (Maybe FilePath, CommandOpt, [ShakeFlag])
opts = info args mempty
  where
    args = (,,) <$> stackYamlFlag <*> pierCmd <*> shakeFlags

    stackYamlFlag = optional $ strOption (long "stack-yaml" <> metavar "YAML")

findStackYamlFile :: Maybe FilePath -> IO FilePath
findStackYamlFile (Just f) = return f
findStackYamlFile Nothing = getCurrentDirectory >>= loop
  where
    loop dir = do
        let candidate = dir </> "stack.yaml"
        let parent = takeDirectory dir
        exists <- Directory.doesFileExist candidate
        if
            | exists -> return candidate
            | parent == dir -> error "Couldn't locate stack.yaml file"
            | otherwise -> loop parent

runWithOptions :: CommandOpt -> Rules ()
runWithOptions Clean = cleaning True
runWithOptions CleanAll = do
    liftIO unfreezeArtifacts
    cleaning True
    cleanAll
runWithOptions (Build targets) = do
    cleaning False
    action $ forP targets (uncurry buildTarget)
runWithOptions (Exec (pkg, target) args) = do
    cleaning False
    action $ do
        exe <- buildExeTarget pkg target
        liftIO $ callArtifact (builtExeDataFiles exe) (builtBinary exe) args
runWithOptions (Which (pkg, target)) = do
    cleaning False
    action $ do
        exe <- buildExeTarget pkg target
        -- TODO: nicer output format.
        putNormal $ pathIn (builtBinary exe)

buildExeTarget :: PackageName -> Target -> Action BuiltExecutable
buildExeTarget pkg target = do
    name <- case target of
                TargetExe name -> return name
                TargetAll -> return $ display pkg
                TargetAllExes -> return $ display pkg
                TargetLib -> error "command can't be used with a \"lib\" target"
    buildExecutableNamed pkg name

main :: IO ()
main = do
    (stackYamlOpt, cmdOpt, flags) <- execParser opts
    -- Run relative to the `stack.yaml` file.
    -- TODO: don't rely on setCurrentDirectory; use absolute paths everywhere
    -- in the code.
    (root, stackYamlFile) <- splitFileName <$> findStackYamlFile stackYamlOpt
    setCurrentDirectory root
    withArgs flags $ runPier $ do
        buildPlanRules
        buildPackageRules
        commandRules
        downloadRules
        installGhcRules
        configRules stackYamlFile
        runWithOptions cmdOpt

-- TODO: move into Build.hs
data Target
    = TargetAll
    | TargetLib
    | TargetAllExes
    | TargetExe String
    deriving Show

parseTarget :: Parser (PackageName, Target)
parseTarget = argument (eitherReader readTarget) (metavar "TARGET")
  where
    readTarget :: String -> Either String (PackageName, Target)
    readTarget s = case splitOn ":" s of
        [n] -> (, TargetAll) <$> readPackageName n
        [n, "lib"] -> (, TargetLib) <$> readPackageName n
        [n, "exe"] -> (, TargetAllExes) <$> readPackageName n
        [n, "exe", e] -> (, TargetExe e) <$> readPackageName n
        _ -> Left $ "Error parsing target " ++ show s
    readPackageName n = case simpleParse n of
        Just p -> return p
        Nothing -> Left $ "Error parsing package name " ++ show n

buildTarget :: PackageName -> Target -> Action ()
buildTarget n TargetAll = void $ askMaybeBuiltLibrary n >> buildExecutables n
buildTarget n TargetLib = void $ askBuiltLibrary n
buildTarget n TargetAllExes = void $ buildExecutables n
buildTarget n (TargetExe e) = void $ buildExecutableNamed n e
