module Development.Pier.Core
    ( -- * Build directory
      runPier
    , pierFile
    , cleanAll
      -- * Directory utilities
    , createParentIfMissing
    ) where

import Control.Monad.IO.Class
import Development.Shake
import Development.Shake.FilePath
import System.Directory

pierDir :: FilePath
pierDir = "_pier"

-- TODO: newtype describing inputs/outputs:
pierFile :: FilePattern -> FilePattern
pierFile = (pierDir </>)

runPier :: Rules () -> IO ()
runPier = shakeArgs shakeOptions
                            { shakeFiles = pierDir
                            , shakeProgress = progressSimple
                            , shakeChange = ChangeDigest
                            -- Detect the number of threads:
                            , shakeThreads = 0
                            }

cleanAll :: Rules ()
cleanAll = action $ removeFilesAfter pierDir ["//"]

createParentIfMissing :: MonadIO m => FilePath -> m ()
createParentIfMissing path
    = liftIO $ createDirectoryIfMissing True (takeDirectory path)
