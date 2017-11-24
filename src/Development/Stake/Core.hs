{-# LANGUAGE DeriveAnyClass #-}
module Development.Stake.Core
    ( -- * Build directory
      runStake
    , stakeFile
      -- * Cleaning
    , rerunIfCleaned
    , cleanBuild
    , cleanAll
      -- * Directory utilities
    , createParentIfMissing

    ) where

import Control.Monad.IO.Class
import Development.Shake
import Development.Shake.FilePath
import System.Directory

stakeDir :: FilePath
stakeDir = ".stake"

-- TODO: newtype describing inputs/outputs:
stakeFile :: FilePattern -> FilePattern
stakeFile = (stakeDir </>)

runStake :: Rules () -> IO ()
runStake rules = shakeArgs shakeOptions
                            { shakeFiles = stakeDir
                            , shakeProgress = progressSimple
                            , shakeChange = ChangeDigest
                            -- Detect the number of threads:
                            , shakeThreads = 0
                            }
                        $ cleaner >> rules

cleanBuild, cleanAll :: Rules ()
cleanBuild = action $ removeFilesAfter "." [cleanFile]
cleanAll = action $ removeFilesAfter stakeDir ["//"]

createParentIfMissing :: MonadIO m => FilePath -> m ()
createParentIfMissing path
    = liftIO $ createDirectoryIfMissing True (takeDirectory path)


rerunIfCleaned :: Action ()
rerunIfCleaned = need [cleanFile]

cleanFile :: FilePath
cleanFile = stakeFile "sentinel"

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
