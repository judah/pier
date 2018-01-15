module Pier.Core.Run
    ( -- * Build directory
      runPier
    , pierFile
    , cleanAll
    ) where

import Development.Shake
import Development.Shake.FilePath

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
