-- | A generic approach to building and caching outputs hermetically.
--
-- Output format: .stake/artifact/HASH/path/to/files
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Development.Stake.Command
    ( commandRules
    , Output
    , output
    , prog
    , progWithCwd
    , runCommand
    , runCommandStdout
    , Command
    , Artifact
    , externalFile
    , (/>)
    , relPath
    , readArtifact
    , doesArtifactExist
    , writeArtifact
    , matchArtifactGlob
    ) where

import Crypto.Hash.SHA256
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Shake
import Development.Shake.Classes hiding (hash)
import Development.Shake.FilePath
import GHC.Generics
import System.Directory as Directory
import System.IO.Temp
import System.Posix.Files (createSymbolicLink)
import Distribution.Simple.Utils (matchDirFileGlob)

import Development.Stake.Core
import Development.Stake.Orphans ()
import Development.Stake.Witness

newtype Command = Command [Prog]
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

data Prog = Prog String [String] FilePath -- cwd, relative to root of the sandbox
    deriving (Show, Typeable, Eq, Generic)

instance Hashable Prog
instance Binary Prog
instance NFData Prog

instance Monoid Command where
    Command ps `mappend` Command ps' = Command (ps ++ ps')
    mempty = Command []

instance Semigroup Command

prog :: String -> [String] -> Command
prog = progWithCwd "."

-- TODO: better API
-- Also, more safety...
progWithCwd :: FilePath -> String -> [String] -> Command
progWithCwd cwd p as = Command [Prog p as cwd]

data Output a = Output [FilePath] (Hash -> a)

instance Functor Output where
    fmap f (Output g h) = Output g (f . h)

instance Applicative Output where
    pure = Output [] . const
    Output f g <*> Output f' g' = Output (f ++ f') (g <*> g')

output :: FilePath -> Output Artifact
output f = Output [f] $ flip Artifact f . Built

-- | Unique identifier of a command
newtype Hash = Hash B.ByteString
    deriving (Show, Eq, Ord, Binary, NFData, Hashable, Generic)

makeHash :: String -> Hash
makeHash = Hash . fixBase64Path . encode . hash . T.encodeUtf8 . T.pack
  where
    -- Remove slashes, since the strings will appear in filepaths.
    -- Also remove some other characters to reduce shell errors.
    fixBase64Path = BC.map $ \case
                                '/' -> '-'
                                '+' -> '.'
                                '=' -> '_'
                                c -> c

hashDir :: Hash -> FilePath
hashDir h = stakeFile $ "artifact" </> hashString h

hashString :: Hash -> String
hashString (Hash h) = BC.unpack h

data Artifact = Artifact Source FilePath
    deriving (Show, Eq, Ord, Generic)

instance Hashable Artifact
instance Binary Artifact
instance NFData Artifact

data Source = Built Hash | External
    deriving (Show, Eq, Ord, Generic)

instance Hashable Source
instance Binary Source
instance NFData Source

externalFile :: FilePath -> Artifact
externalFile = Artifact External

(/>) :: Artifact -> FilePath -> Artifact
Artifact source f /> g = Artifact source (f </> g)

-- TODO: go back to </> for artifacts (or some one-sided operator),
-- and add a check that no two inputs for the same Command are
-- subdirs of each other

data CommandQ = CommandQ
    { _commandQCmd :: Command
    , commandQInputs :: [Artifact]
    , _commandQOutputs :: [FilePath]
    }
    deriving (Show, Eq, Generic)

instance Hashable CommandQ
instance Binary CommandQ
instance NFData CommandQ

type instance RuleResult CommandQ = Hash

-- TODO: sanity-check filepaths; for example, normalize, should be relative, no
-- "..", etc.
commandHash :: CommandQ -> Action Hash
commandHash cmdQ = do
    let externalFiles = [f | Artifact External f <- commandQInputs cmdQ]
    need externalFiles
    -- TODO: streaming hash
    userFileHashes <- liftIO $ map hash <$> mapM B.readFile externalFiles
    return . makeHash
        $ "commandHash: " ++ show (cmdQ, userFileHashes)

runCommand :: Output t -> Set.Set Artifact -> Command -> Action t
runCommand (Output outs mk) inputs c
    = mk <$> askWitness (CommandQ c (Set.toList inputs) outs)

-- TODO: this doesn't work with multiple progs within a command
runCommandStdout :: Set.Set Artifact -> Command -> Action String
runCommandStdout inputs c = do
    out <- runCommand (output stdoutPath) inputs c
    liftIO $ readFile $ artifactRealPath out

-- TODO: come up with a better story around cleaning/rebuilds.
-- (See also comments about removing the directory in `commandRules`.)
-- Maybe: don't use witnesses; instead, just look for the hash to be present
-- to decide whether to re-run things (similar to how oracles work).

-- TODO: make sure no artifact is a subdir of another artifact.

commandRules :: Rules ()
commandRules = addWitness $ \cmdQ@(CommandQ (Command progs) inps outs) -> do
    h <- commandHash cmdQ
    let dir = hashDir h
    liftIO $ createParentIfMissing dir
    -- Explicitly create the directory; if it already exists, throw an
    -- exception since something's gone wrong.  (TODO: better error message)
    liftIO $ createDirectory dir
    -- Make sure to clean up this directory if the command fails.
    flip actionOnException (removeDirectoryRecursive dir) $ do
        tmp <- liftIO $ getCanonicalTemporaryDirectory >>= flip createTempDirectory
                                                        (hashString h)
        checkAllDistinctPaths inps
        liftIO $ mapM_ (linkArtifact tmp) inps
        mapM_ (createParentIfMissing . (tmp </>)) $ outs
        -- TODO: more hermetic around env vars
        let unStdout (Stdout out) = out
        let run (Prog p as cwd) = do
                    -- hack around shake weirdness w.r.t. relative binary paths
                    let p' = if take 2 p == "./"
                                then tmp </> cwd </> p
                                else p
                    quietly $ unStdout <$> command [Cwd $ tmp </> cwd] p' as
        out <- B.concat <$> mapM run progs
        liftIO $ B.writeFile (tmp </> stdoutPath) out
        mapM_ (createParentIfMissing . (dir </>)) outs
        liftIO $ mapM_ (\f -> renameAndFreezeFile (tmp </> f) (dir </> f)) outs
        -- Clean up the temp directory, but only if the above commands succeeded.
        liftIO $ removeDirectoryRecursive tmp
        return h

stdoutPath :: FilePath
stdoutPath = ".stdout"

checkAllDistinctPaths :: [Artifact] -> Action ()
checkAllDistinctPaths as =
    case Map.keys $ Map.filter (> 1) $ Map.fromListWith (+) $ map (,1 :: Integer) $ map relPath as of
        [] -> return ()
        -- TODO: nicer error, telling where they came from:
        fs -> error $ "Artifacts generated from more than one command: " ++ show fs

renameAndFreezeFile :: FilePath -> FilePath -> IO ()
renameAndFreezeFile src dest = do
    let freeze f = getPermissions f >>= setPermissions f . setOwnerWritable False
    forFileRecursive_ freeze src
    renamePath src dest

-- TODO: don't loop on symlinks, and be more efficient?
forFileRecursive_ :: (FilePath -> IO ()) -> FilePath -> IO ()
forFileRecursive_ act f = do
    isDir <- Directory.doesDirectoryExist f
    if not isDir
        then act f
        else do
            fs <- getUsefulContents f
            mapM_ (forFileRecursive_ act) $ map (f </>) fs

getUsefulContents :: FilePath -> IO [FilePath]
getUsefulContents f
    = filter (not . specialFile) <$> Directory.getDirectoryContents f
  where
    specialFile "." = True
    specialFile ".." = True
    specialFile _ = False

linkArtifact :: FilePath -> Artifact -> IO ()
linkArtifact destDir a = do
    curDir <- getCurrentDirectory
    let localPath = destDir </> relPath a
    let realPath = curDir </> artifactRealPath a
    createParentIfMissing localPath
    loop realPath localPath
  where
    loop realPath localPath = do
        isFile <- Directory.doesFileExist realPath
        isDir <- Directory.doesDirectoryExist realPath
        if | isFile -> do
                createSymbolicLink realPath localPath
           | isDir -> do
                        createDirectoryIfMissing False localPath
                        getUsefulContents realPath
                            >>= mapM_ (\f -> loop (realPath </> f)
                                      (localPath </> f))
           | otherwise -> error $ "linkArtifact: does not exist: " ++ realPath


-- TODO: use permissions and/or sandboxing to make this more robust
artifactRealPath :: Artifact -> FilePath
artifactRealPath (Artifact External f) = f
artifactRealPath (Artifact (Built h) f) = hashDir h </> f

readArtifact :: Artifact -> Action String
readArtifact (Artifact External f) = readFile' f -- includes need
readArtifact f = liftIO $ readFile $ artifactRealPath f

relPath :: Artifact -> FilePath
relPath (Artifact _ f) = f

writeArtifact :: MonadIO m => FilePath -> String -> m Artifact
writeArtifact path contents = liftIO $ do
    let h = makeHash $ "writeArtifact: " ++ contents
    let dir = hashDir h
    -- TODO: remove if it already exists?  Should this be Action?
    createParentIfMissing (dir </> path)
    writeFile (dir </> path) contents
    return $ Artifact (Built h) path

-- I guess we need doesFileExist?  Can we make that robust?
doesArtifactExist :: Artifact -> Action Bool
doesArtifactExist (Artifact External f) = Development.Shake.doesFileExist f
doesArtifactExist f = liftIO $ Directory.doesFileExist (artifactRealPath f)

matchArtifactGlob :: Artifact -> FilePath -> Action [Artifact]
-- TODO: match the behavior of Cabal
matchArtifactGlob (Artifact External f) g
    = map (Artifact External . (f </>)) <$> getDirectoryFiles f [g]
matchArtifactGlob a@(Artifact (Built h) f) g
    = fmap (map (Artifact (Built h) . (f </>)))
            $ liftIO $ matchDirFileGlob (artifactRealPath a) g
