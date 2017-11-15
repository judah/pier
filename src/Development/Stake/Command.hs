-- | A generic approach to building and caching outputs hermetically.
--
-- Output format: .stake/artifact/HASH/path/to/files
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module Development.Stake.Command
    ( commandRules
    , Output
    , output
    , prog
    , runCommand
    , runCommandStdout
    , Command
    , Artifact(..)
    , BuiltArtifact
    , relPath
    , writeArtifact
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
import System.Directory
import System.IO.Temp
import System.Posix.Files (createSymbolicLink)

import Development.Stake.Core
import Development.Stake.Witness

-- TODO: avoid this orphan?
instance Hashable a => Hashable (Set.Set a) where
    hashWithSalt k = hashWithSalt k . Set.toList

newtype Command = Command {cmdProgs :: [(String,[String])]}
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

instance Monoid Command where
    Command ps `mappend` Command ps' = Command (ps ++ ps')
    mempty = Command []

instance Semigroup Command

prog :: String -> [String] -> Command
prog p as = Command [(p,as)]

data Output a = Output [FilePath] (Hash -> a)

instance Functor Output where
    fmap f (Output g h) = Output g (f . h)

instance Applicative Output where
    pure = Output [] . const
    Output f g <*> Output f' g' = Output (f ++ f') (g <*> g')

output :: FilePath -> Output Artifact
output f = Output [f] $ Built . flip BuiltArtifact f

-- | Unique identifier of a command
newtype Hash = Hash B.ByteString
    deriving (Show, Eq, Ord, Binary, NFData, Hashable, Generic)

makeHash :: String -> Hash
makeHash = Hash . fixBase64Path . encode . hash . T.encodeUtf8 . T.pack
  where
    -- Remove slashes, since the strings will appear in filepaths.
    fixBase64Path = BC.map $ \case
                                '/' -> '-'
                                c -> c

hashDir :: Hash -> FilePath
hashDir h = artifact $ "archive" </> hashString h

hashString :: Hash -> String
hashString (Hash h) = BC.unpack h

data BuiltArtifact = BuiltArtifact Hash FilePath
    deriving (Show, Eq, Ord, Generic)

instance Hashable BuiltArtifact
instance Binary BuiltArtifact
instance NFData BuiltArtifact

data Artifact = Built BuiltArtifact | UserFile FilePath
    deriving (Show, Eq, Ord, Generic)

instance Hashable Artifact
instance Binary Artifact
instance NFData Artifact

data CommandQ = CommandQ
    { commandQCmd :: Command
    , commandQInputs :: [Artifact]
    , commandQOutputs :: [FilePath]
    }
    deriving (Show, Eq, Generic)

instance Hashable CommandQ
instance Binary CommandQ
instance NFData CommandQ

type instance RuleResult CommandQ = Hash

-- TODO: sanity-check filepaths; for example, normalize, should be relative, no
-- "..", etc.
commandHash :: CommandQ -> Action Hash
commandHash (CommandQ progs inps outputs) = do
    let userFiles = [f | UserFile f <- inps]
    need userFiles
    -- TODO: streaming hash
    userFileHashes <- liftIO $ map hash <$> mapM B.readFile userFiles
    return . makeHash
        $ "commandHash: " ++ show (progs, inps, userFileHashes, outputs)

runCommand :: Output t -> Set.Set Artifact -> Command -> Action t
runCommand (Output outs mk) inputs cmd
    = mk <$> askWitness (CommandQ cmd (Set.toList inputs) outs)

-- TODO: this doesn't work with multiple progs within a command
runCommandStdout :: Set.Set Artifact -> Command -> Action String
runCommandStdout inputs cmd = do
    out <- runCommand (output stdoutPath) inputs cmd
    liftIO $ readFile $ artifactRealPath out

-- TODO: come up with a better story around cleaning/rebuilds.
-- (See also comments about removing the directory in `commandRules`.)
-- Maybe: don't use witnesses; instead, just look for the hash to be present
-- to decide whether to re-run things (similar to how oracles work).

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
        out <- quietly $ B.concat . map unStdout
                    <$> mapM (uncurry $ command [Cwd tmp]) progs
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
    case Map.keys $ Map.filter (> 1) $ Map.fromListWith (+) $ map (,1) $ map relPath as of
        [] -> return ()
        -- TODO: nicer error, telling where they came from:
        fs -> error $ "Artifacts generated from more than one command: " ++ show fs

renameAndFreezeFile :: FilePath -> FilePath -> IO ()
renameAndFreezeFile src dest = do
    getPermissions src >>= setPermissions src . setOwnerWritable False
    renameFile src dest

linkArtifact :: FilePath -> Artifact -> IO ()
linkArtifact dir a = do
    curDir <- getCurrentDirectory
    createParentIfMissing (dir </> artifactLocalPath a)
    createSymbolicLink (curDir </> artifactRealPath a) (dir </> artifactLocalPath a)
  where
    artifactLocalPath :: Artifact -> FilePath
    artifactLocalPath (UserFile f) = f
    artifactLocalPath (Built (BuiltArtifact _ f)) = f


-- TODO: use permissions and/or sandboxing to make this more robust
artifactRealPath :: Artifact -> FilePath
artifactRealPath (UserFile f) = f
artifactRealPath (Built (BuiltArtifact h' f)) = hashDir h' </> f

relPath :: Artifact -> FilePath
relPath (UserFile f) = f
relPath (Built (BuiltArtifact _ f)) = f

writeArtifact :: MonadIO m => FilePath -> String -> m Artifact
writeArtifact path contents = liftIO $ do
    let h = makeHash $ "writeArtifact: " ++ contents
    let dir = hashDir h
    -- TODO: remove if it already exists?  Should this be Action?
    createParentIfMissing (dir </> path)
    writeFile (dir </> path) contents
    return $ Built $ BuiltArtifact h path
