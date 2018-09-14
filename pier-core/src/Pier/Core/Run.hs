module Pier.Core.Run
    ( -- * Build directory
      runPier
    , cleanAll
    ) where

import Development.Shake

import Pier.Core.Internal.Store

runPier :: Rules () -> IO ()
runPier = shakeArgs shakeOptions
                            { shakeFiles = pierDir
                            , shakeProgress = progressSimple
                            , shakeChange = ChangeDigest
                            -- Detect the number of threads:
                            , shakeThreads = 0
                            }

cleanAll :: Rules ()
cleanAll = action $ do
            putNormal $ "Removing " ++ pierDir
            removeFilesAfter pierDir ["//"]


