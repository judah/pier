module Development.Stake.Core
    ( -- * Build directory
      runStake
    , stakeFile
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
runStake = shakeArgs shakeOptions
                            { shakeFiles = stakeDir
                            , shakeProgress = progressSimple
                            , shakeChange = ChangeDigest
                            -- Detect the number of threads:
                            , shakeThreads = 0
                            }

cleanAll :: Rules ()
cleanAll = action $ removeFilesAfter stakeDir ["//"]

createParentIfMissing :: MonadIO m => FilePath -> m ()
createParentIfMissing path
    = liftIO $ createDirectoryIfMissing True (takeDirectory path)
