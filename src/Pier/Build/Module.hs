module Pier.Build.Module
    ( findModule
    , findMainFile
    , findBootFile
    , sourceDirArtifacts
    ) where

import Control.Applicative ((<|>))
import Control.Monad (guard, msum)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.List (intercalate)
import Development.Shake
import Distribution.ModuleName
import Distribution.Package (PackageIdentifier(..), mkPackageName)
import Distribution.PackageDescription
import Distribution.Version (versionNumbers)
import Development.Shake.FilePath
import Distribution.Text (display)

import qualified Data.Set as Set

import Pier.Build.ConfiguredPackage
import Pier.Build.Executable
import Pier.Build.CFlags
import Pier.Build.Stackage
import Pier.Core.Artifact

findModule
    :: InstalledGhc
    -> ConfiguredPackage
    -> CFlags
    -> [Artifact]   -- ^ Source directory to check
    -> ModuleName
    -> Action Artifact
findModule ghc confd flags paths m = do
    found <- runMaybeT $ genPathsModule m confd <|>
                msum (map (search ghc flags m) paths)
    maybe (error $ "Missing module " ++ display m
                    ++ "; searched " ++ show paths)
        return found

findMainFile
    :: InstalledGhc
    -> CFlags
    -> [Artifact]  -- ^ Source directory to check
    -> FilePath
    -> Action Artifact
findMainFile ghc flags paths f = do
    found <- runMaybeT $ msum $
                map findFileDirectly paths ++
                map (search ghc flags $ filePathToModule f) paths
    maybe (error $ "Missing main file " ++ f
                    ++ "; searched " ++ show paths)
        return found
  where
    findFileDirectly path = do
        let candidate = path /> f
        exists candidate
        return candidate

genPathsModule
    :: ModuleName -> ConfiguredPackage -> MaybeT Action Artifact
genPathsModule m confd = do
    guard $ m == pathsModule
    lift $ writeArtifact ("paths" </> display m <.> "hs") $ unlines
        [ "{-# LANGUAGE CPP #-}"
        , "{-# LANGUAGE ImplicitPrelude #-}"
        , "module " ++ display m ++ " (getDataFileName, getDataDir, version) where"
        , "import Data.Version (Version(..))"
        , "version = Version " ++ show (versionNumbers
                                            $ pkgVersion pkg)
                                ++ ""
                        ++ " []" -- tags are deprecated
        , "getDataFileName :: FilePath -> IO FilePath"
        , "getDataFileName f = (\\d -> d ++ \"/\" ++ f) <$> getDataDir"
        , "getDataDir :: IO FilePath"
        , "getDataDir = " ++ maybe err (("return " ++) . show . pathIn)
                                (confdDataFiles confd)
        ]
  where
    pkg = package (confdDesc confd)
    pathsModule = fromString $ "Paths_" ++ map fixHyphen (display $ pkgName pkg)
    fixHyphen '-' = '_'
    fixHyphen c = c
    err = "error " ++ show ("Missing data files from package " ++ display pkg)


search
    :: InstalledGhc
    -> CFlags
    -> ModuleName
    -> Artifact -- ^ Source directory to check
    -> MaybeT Action Artifact
search ghc flags m srcDir
    = genHsc2hs <|>
      genHappy "y" <|>
      genHappy "ly" <|>
      genAlex "x" <|>
      genC2hs <|>
      existing "lhs" <|>
      existing "hs"
  where
    existing ext = let f = srcDir /> toFilePath m <.> ext
                 in exists f >> return f

    genHappy ext = do
        let yFile = srcDir /> toFilePath m <.> ext
        exists yFile
        let relOutput = toFilePath m <.> "hs"
        happy <- lift $ askBuiltExecutable (mkPackageName "happy") "happy"
        lift . runCommand (output relOutput)
             $ progBinary happy
                     ["-agc", "-o", relOutput, pathIn yFile]
                <> input yFile

    genHsc2hs = do
        let hsc = srcDir /> toFilePath m <.> "hsc"
        exists hsc
        let relOutput = toFilePath m <.> "hs"
        lift $ runCommand (output relOutput)
             $ hsc2hsProg ghc
                      (["-o", relOutput
                       , pathIn hsc
                       ]
                       ++ ["--cflag=" ++ f | f <- ccFlags flags
                                                    ++ cppFlags flags]
                       ++ ["-I" ++ pathIn f | f <- Set.toList $ cIncludeDirs flags]
                       ++ ghcDefines ghc)
                <> input hsc <> inputs (cIncludeDirs flags)

    genAlex ext = do
        let xFile = srcDir /> toFilePath m <.> ext
        exists xFile
        let relOutput = toFilePath m <.> "hs"
        -- TODO: mkPackageName doesn't exist in older ones
        alex <- lift $ askBuiltExecutable (mkPackageName "alex") "alex"
        lift . runCommand (output relOutput)
            $ progBinary alex
                     ["-g", "-o", relOutput, pathIn xFile]
               <> input xFile
    genC2hs = do
        let chsFile = srcDir /> toFilePath m <.> "chs"
        exists chsFile
        let relOutput = toFilePath m <.> "hs"
        c2hs <- lift $ askBuiltExecutable (mkPackageName "c2hs") "c2hs"
        lift . runCommand (output relOutput)
             $ input chsFile
            <> inputs (cIncludeDirs flags)
            <> progBinary c2hs
                    (["-o", relOutput, pathIn chsFile]
                    ++ ["--include=" ++ pathIn f | f <- Set.toList (cIncludeDirs flags)]
                    ++ ["--cppopts=" ++ f | f <- ccFlags flags ++ cppFlags flags
                                                    ++ ghcDefines ghc]
                    )
-- TODO: issue if this doesn't preserve ".lhs" vs ".hs", for example?
filePathToModule :: FilePath -> ModuleName
filePathToModule = fromString . intercalate "." . splitDirectories . dropExtension

exists :: Artifact -> MaybeT Action ()
exists f = lift (doesArtifactExist f) >>= guard

-- Find the "hs-boot" file corresponding to a "hs" file.
findBootFile :: Artifact -> Action (Maybe Artifact)
findBootFile hs = do
    let hsBoot = replaceArtifactExtension hs "hs-boot"
    bootExists <- doesArtifactExist hsBoot
    return $ guard bootExists >> return hsBoot

sourceDirArtifacts :: Artifact -> BuildInfo -> [Artifact]
sourceDirArtifacts packageSourceDir bi
    = map (packageSourceDir />) $ ifNullDirs $ hsSourceDirs bi

-- TODO: Organize the arguments to this function better.
ifNullDirs :: [FilePath] -> [FilePath]
ifNullDirs [] = [""]
ifNullDirs xs = xs
