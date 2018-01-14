-- | A generic approach to building and caching outputs hermetically.
--
-- Output format: _pier/artifact/HASH/path/to/files
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
module Development.Pier.Command
    ( commandRules
    , Output
    , output
    , prog
    , progA
    , progTemp
    , input
    , inputs
    , inputList
    , message
    , withCwd
    , runCommand
    , runCommandStdout
    , runCommand_
    , Command
    , Artifact
    , externalFile
    , (/>)
    , pathIn
    , pathOut
    , replaceArtifactExtension
    , readArtifact
    , readArtifactB
    , doesArtifactExist
    , writeArtifact
    , matchArtifactGlob
    , unfreezeArtifacts
    , shadow
    , callArtifact
    , createDirectoryA
    ) where

import Crypto.Hash.SHA256
import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Shake
import Development.Shake.Classes hiding (hash)
import Development.Shake.FilePath
import GHC.Generics
import System.Directory as Directory
import System.IO.Temp
import System.Process (showCommandForUser)
import System.Posix.Files (createSymbolicLink)
import Distribution.Simple.Utils (matchDirFileGlob)

import Development.Pier.Core
import Development.Pier.Orphans ()
import Development.Pier.Persistent

-- TODO: reconsider names in this module

data Command = Command
    { _commandProgs :: [Prog]
    , commandInputs :: Set Artifact
    }
    deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

data Call
    = CallEnv String -- picked up from $PATH
    | CallArtifact Artifact
    | CallTemp FilePath -- Local file to this Command
                        -- (e.g. generated by an earlier call)
                        -- (This is a hack around shake which tries to resolve
                        -- local files in the env.)
    deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

data Prog
    = Prog { progCall :: Call
           , progArgs :: [String]
           , progCwd :: FilePath  -- relative to the root of the sandbox
           }
    | Message String
    | Shadow Artifact FilePath
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)

instance Show Prog where
    show (Shadow f g) = "(Shadow " ++ show f ++ " " ++ show g ++ ")"
    show (Message s) = "(Message " ++ show s ++ ")"
    show p@Prog{} = "(" ++ maybeCd
                ++ showCommandForUser (showCall $ progCall p) (progArgs p) ++ ")"
      where
        maybeCd
            | progCwd p == "." = ""
            | otherwise = "cd " ++ show (progCwd p) ++ " && "
        showCall (CallArtifact a) = pathIn a
        -- TODO: this doesn't fully distinguish env and temp...
        showCall (CallEnv f) = f
        showCall (CallTemp f) = f
    showList [] = id
    showList (p:ps) = shows p . showString " && " . showList ps

instance Monoid Command where
    Command ps is `mappend` Command ps' is' = Command (ps ++ ps') (is <> is')
    mempty = Command [] Set.empty

instance Semigroup Command

-- TODO: allow prog taking Artifact and using it as input

prog :: String -> [String] -> Command
prog p as = Command [Prog (CallEnv p) as "."] Set.empty

progA :: Artifact -> [String] -> Command
progA p as = Command [Prog (CallArtifact p) as "."] (Set.singleton p)

progTemp :: FilePath -> [String] -> Command
progTemp p as = Command [Prog (CallTemp p) as "."] Set.empty

message :: String -> Command
message s = Command [Message s] Set.empty

withCwd :: FilePath -> Command -> Command
withCwd path (Command ps as)
    | isAbsolute path = error $ "withCwd: expected relative path, got " ++ show path
    | otherwise = Command (map setPath ps) as
  where
    setPath m@Message{} = m
    setPath p = p { progCwd = path }

input :: Artifact -> Command
input = inputs . Set.singleton

inputList :: [Artifact] -> Command
inputList = inputs . Set.fromList

inputs :: Set Artifact -> Command
inputs = Command []

-- | Make a "shadow" copy of the given input artifact's by create a symlink of
-- this artifact (if it is a file) or of each sub-file (transitively, if it is
-- a directory).
--
-- The result may be captured as output, for example when grouping multiple outputs
-- of separate commands into a common directory structure.
shadow :: Artifact -> FilePath -> Command
shadow a f
    | isAbsolute f = error $ "shadowArtifact: need relative destination, found "
                            ++ show f
    | otherwise = Command [Shadow a f] Set.empty

data Output a = Output [FilePath] (Hash -> a)

instance Functor Output where
    fmap f (Output g h) = Output g (f . h)

instance Applicative Output where
    pure = Output [] . const
    Output f g <*> Output f' g' = Output (f ++ f') (g <*> g')

output :: FilePath -> Output Artifact
output f
    | normalise f == "." = error $ "Can't output empty path " ++ show f
    | isAbsolute f = error $ "Can't output absolute path " ++ show f
    | otherwise = Output [f] $ flip Artifact (normalise f) . Built

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
hashDir h = artifactDir </> hashString h

artifactDir :: FilePath
artifactDir = pierFile "artifact"

hashString :: Hash -> String
hashString (Hash h) = BC.unpack h

data Artifact = Artifact Source FilePath
    deriving (Eq, Ord, Generic)

instance Show Artifact where
    show (Artifact External f) = "external:" ++ show f
    show (Artifact (Built h) f) = hashString h ++ ":" ++ show f

instance Hashable Artifact
instance Binary Artifact
instance NFData Artifact

data Source = Built Hash | External
    deriving (Show, Eq, Ord, Generic)

instance Hashable Source
instance Binary Source
instance NFData Source

externalFile :: FilePath -> Artifact
externalFile = Artifact External . normalise

(/>) :: Artifact -> FilePath -> Artifact
Artifact source f /> g = Artifact source $ normalise $ f </> g

infixr 5 />  -- Same as </>

-- TODO: go back to </> for artifacts (or some one-sided operator),
-- and add a check that no two inputs for the same Command are
-- subdirs of each other

data CommandQ = CommandQ
    { commandQCmd :: Command
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
    let externalFiles = [f | Artifact External f <- Set.toList $ commandInputs
                                                        $ commandQCmd cmdQ]
    need externalFiles
    -- TODO: streaming hash
    userFileHashes <- liftIO $ map hash <$> mapM B.readFile externalFiles
    return . makeHash
        $ "commandHash: " ++ show (cmdQ, userFileHashes)

runCommand :: Output t -> Command -> Action t
runCommand (Output outs mk) c
    = mk <$> askPersistent (CommandQ c outs)

runCommandStdout :: Command -> Action String
runCommandStdout c = do
    out <- runCommand (output stdoutPath) c
    liftIO $ readFile $ pathIn out

runCommand_ :: Command -> Action ()
runCommand_ = runCommand (pure ())

-- TODO: come up with a better story around cleaning/rebuilds.
-- (See also comments about removing the directory in `commandRules`.)
-- Maybe: don't use Persistent; instead, just look for the hash to be present
-- to decide whether to re-run things (similar to how oracles work).

-- TODO: make sure no artifact is a subdir of another artifact.

-- TODO: directories within archives are writable, and are modifyable
-- through symlinks.  Either just always do a `lndir`, or use real
-- sandboxes.

commandRules :: Rules ()
commandRules = addPersistent $ \cmdQ@(CommandQ (Command progs inps) outs) -> do
    h <- commandHash cmdQ
    let outDir = hashDir h
    -- Skip if the output directory already exists; we'll produce it atomically
    -- below.  This could happen if the action stops before Shake registers it as
    -- complete, due to either a synchronous or asynchronous exception.
    exists <- liftIO $ Directory.doesDirectoryExist outDir
    unless exists $ do
        tmp <- liftIO $ getCanonicalTemporaryDirectory >>= flip createTempDirectory
                                                        (hashString h)
        let tmpOutPath = (tmp </>) . pathOut
        liftIO $ collectInputs inps tmp
        mapM_ (createParentIfMissing . tmpOutPath) outs

        out <- B.concat <$> mapM (readProg tmp) progs
        createParentIfMissing $ tmpOutPath stdoutPath
        liftIO $ B.writeFile (tmpOutPath stdoutPath) out

        liftIO $ forM_ outs $ \f -> do
                        exist <- Directory.doesPathExist (tmpOutPath f)
                        unless exist $
                            error $ "runCommand: missing output "
                                    ++ show f
                                    ++ " in temporary directory "
                                    ++ show tmp
        liftIO $ withSystemTempDirectory (hashString h) $ \tempOutDir -> do
            mapM_ (createParentIfMissing . (tempOutDir </>)) outs
            forM_ outs
                $ \f -> renamePath (tmpOutPath f)
                                    (tempOutDir </> f)
            finalizeFrozen tempOutDir outDir
        -- Clean up the temp directory, but only if the above commands succeeded.
        liftIO $ removeDirectoryRecursive tmp
    return h

pathOut :: FilePath -> FilePath
pathOut f = artifactDir </> "out" </> f

-- TODO: more hermetic?
collectInputs :: Set Artifact -> FilePath -> IO ()
collectInputs inps tmp = do
    let inps' = dedupArtifacts inps
    checkAllDistinctPaths inps'
    liftIO $ mapM_ (linkArtifact tmp) inps'

-- Move the source directory to the destination and make it read-only.
finalizeFrozen :: FilePath -> FilePath -> IO ()
finalizeFrozen src dest = do
    getRegularContents src
        >>= mapM_ (forFileRecursive_ freezePath . (src </>))
    createParentIfMissing dest
    -- Make the output directory appear atomically (see above).
    Directory.renameDirectory src dest
    -- Freeze the actual directory after doing the move, which requires
    -- write permissions.
    freezePath dest

-- Call a process inside the given directory and capture its stdout.
-- TODO: more flexibility around the env vars
-- Also: limit valid parameters for the *prog* binary (rather than taking it
-- from the PATH that the `pier` executable sees).
readProg :: FilePath -> Prog -> Action B.ByteString
readProg _ (Message s) = putNormal s >> return B.empty
readProg dir (Prog p as cwd) = do
    let unStdout (Stdout out) = out
    -- hack around shake weirdness w.r.t. relative binary paths
    let p' = case p of
                CallEnv s -> s
                CallArtifact f -> dir </> pathIn f
                CallTemp f -> dir </> pathOut f
    quietly $ unStdout
            <$> command
                    [ Cwd $ dir </> cwd
                    , Env defaultEnv
                    -- stderr will get printed if there's an error.
                    , EchoStderr False
                    ]
                    p' (map (spliceTempDir dir) as)
readProg dir (Shadow a0 f0) = liftIO $ do
    let out = dir </> pathOut f0
    createParentIfMissing out
    rootDir <- Directory.getCurrentDirectory
    deepLink (rootDir </> pathIn a0) out
    return B.empty
  where
    deepLink a f = do
        isDir <- Directory.doesDirectoryExist a
        if isDir
            then do
                    Directory.createDirectoryIfMissing False f
                    cs <- getRegularContents a
                    mapM_ (\c -> deepLink (a </> c) (f </> c)) cs
            else createSymbolicLink a f

stdoutPath :: FilePath
stdoutPath = "_stdout"

defaultEnv :: [(String, String)]
defaultEnv = [("PATH", "/usr/bin:/bin")]

spliceTempDir :: FilePath -> String -> String
spliceTempDir tmp = T.unpack . T.replace (T.pack "${TMPDIR}") (T.pack tmp) . T.pack

checkAllDistinctPaths :: Monad m => [Artifact] -> m ()
checkAllDistinctPaths as =
    case Map.keys . Map.filter (> 1) . Map.fromListWith (+)
            . map (\a -> (pathIn a, 1 :: Integer)) $ as of
        [] -> return ()
        -- TODO: nicer error, telling where they came from:
        fs -> error $ "Artifacts generated from more than one command: " ++ show fs

-- Remove duplicate artifacts that are both outputs of the same command, and where
-- one is a subdirectory of the other (for example, constructed via `/>`).
dedupArtifacts :: Set Artifact -> [Artifact]
dedupArtifacts = loop . Set.toAscList
  where
    -- Loop over artifacts built from the same command.
    -- toAscList plus lexicographic sorting means that
    -- subdirectories with the same hash will appear consecutively after directories
    -- that contain them.
    loop (a@(Artifact (Built h) f) : Artifact (Built h') f' : fs)
        | h == h', (f <//> "*") ?== f' = loop (a:fs)
    loop (f:fs) = f : loop fs
    loop [] = []

freezePath :: FilePath -> IO ()
freezePath f = getPermissions f >>= setPermissions f . setOwnerWritable False

-- | Make all artifacts user-writable, so they can be deleted by `clean-all`.
unfreezeArtifacts :: IO ()
unfreezeArtifacts = do
    exists <- Directory.doesDirectoryExist artifactDir
    when exists $ forFileRecursive_ unfreeze artifactDir
  where
    unfreeze f = do
        sym <- pathIsSymbolicLink f
        unless sym $ getPermissions f >>= setPermissions f . setOwnerWritable True

-- TODO: don't loop on symlinks, and be more efficient?
forFileRecursive_ :: (FilePath -> IO ()) -> FilePath -> IO ()
forFileRecursive_ act f = do
    isDir <- Directory.doesDirectoryExist f
    if not isDir
        then act f
        else do
            getRegularContents f >>= mapM_ (forFileRecursive_ act . (f </>))
            act f

getRegularContents :: FilePath -> IO [FilePath]
getRegularContents f =
    filter (not . specialFile) <$> Directory.getDirectoryContents f
  where
    specialFile "." = True
    specialFile ".." = True
    specialFile _ = False

-- Symlink the artifact into the given destination directory.
linkArtifact :: FilePath -> Artifact -> IO ()
linkArtifact _ (Artifact External f)
    | isAbsolute f = return ()
linkArtifact dir a = do
    curDir <- getCurrentDirectory
    let realPath = curDir </> pathIn a
    let localPath = dir </> pathIn a
    checkExists realPath
    createParentIfMissing localPath
    createSymbolicLink realPath localPath
  where
    -- Sanity check
    checkExists f = do
        isFile <- Directory.doesFileExist f
        isDir <- Directory.doesDirectoryExist f
        when (not isFile && not isDir)
            $ error $ "linkArtifact: source does not exist: " ++ show f
                        ++ " for artifact " ++ show a


pathIn :: Artifact -> FilePath
pathIn (Artifact External f) = f
pathIn (Artifact (Built h) f) = hashDir h </> f

replaceArtifactExtension :: Artifact -> String -> Artifact
replaceArtifactExtension (Artifact s f) ext
    = Artifact s $ replaceExtension f ext

readArtifact :: Artifact -> Action String
readArtifact (Artifact External f) = readFile' f -- includes need
readArtifact f = liftIO $ readFile $ pathIn f

readArtifactB :: Artifact -> Action B.ByteString
readArtifactB (Artifact External f) = need [f] >> liftIO (B.readFile f)
readArtifactB f = liftIO $ B.readFile $ pathIn f

-- TODO: atomic
writeArtifact :: MonadIO m => FilePath -> String -> m Artifact
writeArtifact path contents = liftIO $ do
    let h = makeHash $ "writeArtifact: " ++ contents
    let dir = hashDir h
    exists <- Directory.doesDirectoryExist dir
    unless exists $ withSystemTempDirectory (hashString h)
                        $ \tempOutDir -> do
        let out = tempOutDir </> path
        createParentIfMissing out
        writeFile out contents
        finalizeFrozen tempOutDir dir
    return $ Artifact (Built h) $ normalise path

-- I guess we need doesFileExist?  Can we make that robust?
doesArtifactExist :: Artifact -> Action Bool
doesArtifactExist (Artifact External f) = Development.Shake.doesFileExist f
doesArtifactExist f = liftIO $ Directory.doesFileExist (pathIn f)

matchArtifactGlob :: Artifact -> FilePath -> Action [Artifact]
-- TODO: match the behavior of Cabal
matchArtifactGlob (Artifact External f) g
    = map (Artifact External . normalise . (f </>)) <$> getDirectoryFiles f [g]
matchArtifactGlob a@(Artifact (Built h) f) g
    = fmap (map (Artifact (Built h) . normalise . (f </>)))
            $ liftIO $ matchDirFileGlob (pathIn a) g

-- TODO: merge more with above code?  How hermetic should it be?
callArtifact :: Set Artifact -> Artifact -> [String] -> IO ()
callArtifact inps bin args = do
    tmp <- liftIO $ getCanonicalTemporaryDirectory >>= flip createTempDirectory
                                                        "exec"
    -- TODO: preserve if it fails?  Make that a parameter?
    collectInputs (Set.insert bin inps) tmp
    cmd_ [Cwd tmp]
        (tmp </> pathIn bin) args
    -- Clean up the temp directory, but only if the above commands succeeded.
    liftIO $ removeDirectoryRecursive tmp

createDirectoryA :: FilePath -> Command
createDirectoryA f = prog "mkdir" ["-p", pathOut f]