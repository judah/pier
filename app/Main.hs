{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import Control.Monad (void)
import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Monoid (Last(..))
import Data.Semigroup (Semigroup, (<>))
import Development.Shake hiding (command)
import Development.Shake.FilePath ((</>), takeDirectory, splitFileName)
import Distribution.Package
import Distribution.Text (display, simpleParse)
import Options.Applicative hiding (action)
import System.Directory as Directory
import System.Environment

import Pier.Build.Config
import Pier.Build.Stackage
import Pier.Build.Components
import Pier.Core.Run
import Pier.Core.Command hiding (runCommand)
import Pier.Core.Download
import Pier.Core.Persistent

data CommandOpt
    = Clean
    | CleanAll
    | Build [(PackageName, Target)]
    | Run Sandboxed (PackageName, Target) [String]
    | Which (PackageName, Target)

data Sandboxed = Sandbox | NoSandbox

parseSandboxed :: Parser Sandboxed
parseSandboxed =
    flag NoSandbox Sandbox
        $ long "sandbox"
        <> help "Run hermetically in a temporary folder"

data CommonOptions = CommonOptions
    { stackYaml :: Last FilePath
    , shakeFlags :: [String]
    }

instance Semigroup CommonOptions where
    CommonOptions y f <> CommonOptions y' f'
        = CommonOptions (y <> y') (f <> f')

-- | Parse command-independent options.
-- 
-- These are allowed both at the top level
-- (for example, "-V" in "pier -V build TARGETS") and within individual
-- commands ("pier build -V TARGETS").  However, we want them to only appear
-- in "pier --help", not "pier build --help".  Doing so is slightly
-- cumbersome with optparse-applicative.
parseCommonOptions :: Hidden -> Parser CommonOptions
parseCommonOptions h = CommonOptions <$> parseStackYaml <*> parseShakeFlags h
  where
    parseStackYaml :: Parser (Last FilePath)
    parseStackYaml = fmap Last $ optional $ strOption
                        $ long "stack-yaml" <> metavar "YAML" <> hide h

data Hidden = Hidden | Shown

hide :: Hidden -> Mod f a
hide Hidden = hidden <> internal
hide Shown = mempty

parseShakeFlags :: Hidden -> Parser [String]
parseShakeFlags h =
    mconcat <$> sequenceA [verbosity, many parallelism, many shakeArg]
  where
    shakeArg = strOption (long "shake-arg" <> metavar "SHAKEARG" <> hide h)

    parallelism =
        fmap ("--jobs=" ++) . strOption
            $ long "jobs"
                <> short 'j'
                <> help "Number of job/threads at once [default CPUs]"
                <> hide h

    verbosity =
        fmap combineFlags . many . flag' 'V'
            $ long "verbose"
                <> short 'V'
                <> help "Increase the verbosity level"
                <> hide h

    combineFlags [] = []
    combineFlags vs = ['-':vs]

parser :: ParserInfo (CommonOptions, CommandOpt)
parser = fmap (\(x,(y,z)) -> (x <> y, z))
            $ info (helper <*> liftA2 (,) (parseCommonOptions Shown)
                                    parseCommand)
            $ progDesc "Yet another Haskell build tool"

parseCommand :: Parser (CommonOptions, CommandOpt)
parseCommand = subparser $ mconcat
    [ make "clean" cleanCommand "Clean project"
    , make "clean-all" cleanAllCommand "Clean project & dependencies"
    , make "build" buildCommand "Build project"
    , make "run" runCommand "Run executable"
    , make "which" whichCommand "Build executable and print its location"
    ]
  where
    make name act desc =
        command name $ info (liftA2 (,) (parseCommonOptions Hidden)
                                        (helper <*> act))
                     $ progDesc desc

cleanCommand :: Parser CommandOpt
cleanCommand = pure Clean

cleanAllCommand :: Parser CommandOpt
cleanAllCommand = pure CleanAll

buildCommand :: Parser CommandOpt
buildCommand = Build <$> many parseTarget

runCommand :: Parser CommandOpt
runCommand = Run <$> parseSandboxed <*> parseTarget
                 <*> many (strArgument (metavar "ARGUMENT"))

whichCommand :: Parser CommandOpt
whichCommand = Which <$> parseTarget


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
    action $ do
        -- Build everything if the targets list is empty
        targets' <- if null targets
                        then map (,TargetAll) . HM.keys . localPackages
                                <$> askConfig
                        else pure targets
        forP targets' (uncurry buildTarget)
runWithOptions (Run sandbox (pkg, target) args) = do
    cleaning False
    action $ do
        exe <- buildExeTarget pkg target
        case sandbox of
            Sandbox -> liftIO $ callArtifact (builtExeDataFiles exe)
                                    (builtBinary exe) args
            NoSandbox -> quietly $ command_ [WithStderr False]
                            (pathIn $ builtBinary exe) args
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
    (commonOpts, cmdOpt) <- execParser parser
    -- Run relative to the `stack.yaml` file.
    -- TODO: don't rely on setCurrentDirectory; use absolute paths everywhere
    -- in the code.
    (root, stackYamlFile)
        <- splitFileName <$> findStackYamlFile (getLast $ stackYaml commonOpts)
    setCurrentDirectory root
    withArgs (shakeFlags commonOpts) $ runPier $ do
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
