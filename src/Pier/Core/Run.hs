module Pier.Core.Run
    ( -- * Build directory
      runPier
    , pierFile
    , cleanAll
    -- * Temporary files and directories
    , HandleTemps(..)
    , withPierTempDirectory
    , withPierTempDirectoryAction
    , createPierTempDirectory
    , createPierTempFile
    ) where

import Control.Monad.IO.Class
import Development.Shake
import Development.Shake.FilePath
import System.Directory
import System.IO.Temp

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
cleanAll = action $ do
            putNormal $ "Removing " ++ pierDir
            removeFilesAfter pierDir ["//"]

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
pierTempDirectory = pierDir </> "tmp"

createPierTempDirectory :: MonadIO m => String -> m FilePath
createPierTempDirectory template = liftIO $ do
    createDirectoryIfMissing True pierTempDirectory
    createTempDirectory pierTempDirectory template

createPierTempFile :: MonadIO m => String -> m FilePath
createPierTempFile template = liftIO $ do
    createDirectoryIfMissing True pierTempDirectory
    writeTempFile pierTempDirectory template ""
