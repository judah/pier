{-# LANGUAGE DeriveAnyClass #-}
module Pier.Core.Internal.Store
    ( -- * Temporary files and directories
      HandleTemps(..),
      withPierTempDirectory,
      withPierTempDirectoryAction,
      pierTempDirectory,
      createPierTempDirectory,
      createPierTempFile,
      -- * Build directory
      pierDir,
      -- * Hash directories
      artifactDir,
      Hash,
      hashString,
      hashDir,
      makeHash,
      createArtifacts,
      unfreezeArtifacts,
      SharedCache(..),
      hashExternalFile,
      -- * Rules
      storeRules,
    ) where

import Control.Monad (forM_, when, void)
import Control.Monad.IO.Class
import Crypto.Hash.SHA256 (hashlazy, hash)
import Data.ByteString.Base64 (encode)
import Development.Shake
import Development.Shake.Classes hiding (hash)
import Development.Shake.FilePath
import GHC.Generics
import System.Directory as Directory
import System.IO.Temp

import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Pier.Core.Internal.Directory

pierDir :: FilePath
pierDir = "_pier"

data HandleTemps = RemoveTemps | KeepTemps

withPierTempDirectoryAction
    :: HandleTemps -> String -> (FilePath -> Action a) -> Action a
withPierTempDirectoryAction KeepTemps template f =
    createPierTempDirectory template >>= f
withPierTempDirectoryAction RemoveTemps template f = do
    tmp <- createPierTempDirectory template
    f tmp `actionFinally` removeDirectoryRecursive tmp

withPierTempDirectory
    :: HandleTemps -> String -> (FilePath -> IO a) -> IO a
withPierTempDirectory KeepTemps template f =
    createPierTempDirectory template >>= f
withPierTempDirectory RemoveTemps template f = do
    createDirectoryIfMissing True pierTempDirectory
    withTempDirectory pierTempDirectory template f

pierTempDirectory :: String
pierTempDirectory = "tmp"

createPierTempDirectory :: MonadIO m => String -> m FilePath
createPierTempDirectory template = liftIO $ do
    createDirectoryIfMissing True pierTempDirectory
    createTempDirectory pierTempDirectory template

createPierTempFile :: MonadIO m => String -> m FilePath
createPierTempFile template = liftIO $ do
    createDirectoryIfMissing True pierTempDirectory
    writeTempFile pierTempDirectory template ""

-- | Unique identifier of a command
newtype Hash = Hash B.ByteString
    deriving (Show, Eq, Ord, Binary, NFData, Hashable, Generic)

makeHash :: Binary a => a -> Action Hash
makeHash x = do
    version <- askOracle GetArtifactVersion
    return . Hash . fixChars . dropPadding . encode . hashlazy . Binary.encode
         . tagVersion version
        $ x
  where
    -- Remove slashes, since the strings will appear in filepaths.
    fixChars = BC.map $ \case
                                '/' -> '_'
                                c -> c
    -- Padding just adds noise, since we don't have length requirements (and indeed
    -- every sha256 hash is 32 bytes)
    dropPadding c
        | BC.last c == '=' = BC.init c
        -- Shouldn't happen since each hash is the same length:
        | otherwise = c
    tagVersion = (,)

hashExternalFile :: FilePath -> IO B.ByteString
hashExternalFile = fmap hash . B.readFile

-- | Version number of artifacts being generated.
newtype ArtifactVersion = ArtifactVersion Int
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)

data GetArtifactVersion = GetArtifactVersion
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData,Generic)
type instance RuleResult GetArtifactVersion = ArtifactVersion

artifactVersionRule :: Rules ()
artifactVersionRule = void $ addOracle $ \GetArtifactVersion
    -- Bumping this will cause every artifact to be regenerated, and should
    -- only be done in case of backwards-incompatible changes.
    -> return $ ArtifactVersion 1

hashDir :: Hash -> FilePath
hashDir h = artifactDir </> hashString h

hashString :: Hash -> String
hashString (Hash h) = BC.unpack h

storeRules :: Rules ()
storeRules = artifactVersionRule

newtype SharedCache = SharedCache FilePath

globalHashDir :: SharedCache -> Hash -> FilePath
globalHashDir (SharedCache f) h = f </> hashString h

-- | Create a directory containing Artifacts.
--
-- If the output directory already exists, don't do anything.  Otherwise, run
-- the given function with a temporary directory, and then move that directory
-- atomically to the final output directory for those Artifacts.
-- Files and (sub)directories, as well as the directory itself, will
-- be made read-only.
createArtifacts ::
       Maybe SharedCache
    -> Hash
    -> [String] -- ^ Messages to print if cached
    -> (FilePath -> Action ())
    -> Action ()
createArtifacts maybeSharedCache h messages act = do
    let destDir = hashDir h
    exists <- liftIO $ Directory.doesDirectoryExist destDir
    -- Skip if the output directory already exists; we'll produce it atomically
    -- below.  This could happen if Shake's database was cleaned, or if the
    -- action stops before Shake registers it as complete, due to either a
    -- synchronous or asynchronous exception.
    if exists
        then mapM_ cacheMessage messages
        else do
            tempDir <- createPierTempDirectory $ hashString h ++ "-result"
            case maybeSharedCache of
                Nothing -> act tempDir
                Just cache -> do
                    getFromSharedCache <- liftIO $ copyFromCache cache h tempDir
                    if getFromSharedCache
                        then mapM_ sharedCacheMessage messages
                        else do
                            act tempDir
                            liftIO $ copyToCache cache h tempDir
            liftIO $ finish tempDir destDir
  where
    cacheMessage m = putNormal $ "(from cache: " ++ m ++ ")"
    sharedCacheMessage m = putNormal $ "(from shared cache: " ++ m ++ ")"
    finish tempDir destDir = do
        -- Move the created directory to its final location,
        -- with all the files and directories inside set to
        -- read-only.
        -- Don't set permissions on symbolic links; they're ignored
        -- on most systems (e.g., Linux).
        let freeze RegularFile = freezePath
            freeze DirectoryEnd = freezePath
            freeze _ = const $ return ()
        -- TODO: why is getRegularContents used?
        -- Ah, to avoid the current directory.
        getRegularContents tempDir
            >>= mapM_ (forFileRecursive_ freeze . (tempDir </>))
        createParentIfMissing destDir
        Directory.renameDirectory tempDir destDir
        -- Also set the directory itself to read-only, but wait
        -- until the last step since read-only files can't be moved.
        freezePath destDir

-- TODO: consider using hard links for these copies, to save space
-- TODO: make sure the directories are read-only
copyFromCache :: SharedCache -> Hash -> FilePath -> IO Bool
copyFromCache cache h tempDir = do
    let globalDir = globalHashDir cache h
    globalExists <- liftIO $ Directory.doesDirectoryExist globalDir
    if globalExists
        then copyDirectory globalDir tempDir >> return True
        else return False

copyToCache :: SharedCache -> Hash -> FilePath -> IO ()
copyToCache cache h src = do
    tempDir <- createPierTempDirectory $ hashString h ++ "-cache"
    copyDirectory src tempDir
    let dest = globalHashDir cache h
    createParentIfMissing dest
    Directory.renameDirectory tempDir dest

artifactDir :: FilePath
artifactDir = pierDir </> "artifact"

freezePath :: FilePath -> IO ()
freezePath f =
    getPermissions f >>= setPermissions f . setOwnerWritable False

-- | Make all artifacts user-writable, so they can be deleted by `clean-all`.
unfreezeArtifacts :: IO ()
unfreezeArtifacts = forM_ [artifactDir, pierTempDirectory] $ \dir -> do
    exists <- Directory.doesDirectoryExist dir
    when exists $ forFileRecursive_ unfreeze dir
  where
    unfreeze DirectoryStart f =
        getPermissions f >>= setPermissions f . setOwnerWritable True
    unfreeze _ _ = return ()
