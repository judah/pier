{-# LANGUAGE DeriveAnyClass #-}
module Development.Stake.Core
    ( -- * Build directory
      runStake
    , artifact
    , (#>)
      -- * Cleaning
    , rerunIfCleaned
    , cleanBuild
    , buildArtifact
    , cleanAll
      -- * Directory utilities
    , createParentIfMissing

    ) where

import Crypto.Hash.SHA256
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Shake
import Development.Shake.Classes hiding (hash)
import Development.Shake.FilePath
import Development.Stake.Witness
import GHC.Generics
import System.Directory
import System.IO.Temp
import System.Posix.Files (createSymbolicLink)

stakeDir :: FilePath
stakeDir = ".stake"

-- TODO: newtype describing inputs/outputs:
artifact :: FilePattern -> FilePattern
artifact = (stakeDir </>)

buildDir :: FilePath
buildDir = "build"

buildArtifact :: FilePattern -> FilePattern
buildArtifact = artifact . (buildDir </>)

(#>) :: FilePath -> (FilePath -> [String] -> Action ()) -> Rules ()
pat #> act = pat' %> \f -> case filePattern pat' f of
                                Just ms -> act f ms
                                Nothing -> fail $ "Shouldn't happen: no match"
                                                ++ " for pattern " ++ show (pat', f)
  where
    pat' = artifact pat
infixl #>

runStake :: Rules () -> IO ()
runStake rules = shakeArgs shakeOptions
                            { shakeFiles = stakeDir
                            , shakeProgress = progressSimple
                            , shakeChange = ChangeDigest
                            -- Detect the number of threads:
                            , shakeThreads = 0
                            }
                        $ cleaner >> rules

runClean :: FilePattern -> Rules ()
runClean pat = action $ removeFilesAfter stakeDir [pat]

cleanBuild, cleanAll :: Rules ()
cleanBuild = runClean buildDir
cleanAll = runClean ""

createParentIfMissing :: MonadIO m => FilePath -> m ()
createParentIfMissing path
    = liftIO $ createDirectoryIfMissing True (takeDirectory path)


rerunIfCleaned :: Action ()
rerunIfCleaned = need [cleanFile]

cleanFile :: FilePath
cleanFile = buildArtifact "sentinel"

cleaner :: Rules ()
cleaner = cleanFile %> \f -> quietly $ do
    Stdout c <- cmd "uuidgen"
    writeFile' f c


{-
data Cleaner = Cleaner
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult Cleaner = Bool

setCleaned :: Bool -> Rules ()
setCleaned c =
-}
