module Pier.Core.Run
    ( -- * Build directory
      runPier
    , pierFile
    , cleanAll
      -- * Display
    , Display
    , withMessage
    ) where

import Control.Concurrent.MVar
import Control.Exception
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Development.Shake
import Development.Shake.FilePath

pierDir :: FilePath
pierDir = "_pier"

-- TODO: newtype describing inputs/outputs:
pierFile :: FilePattern -> FilePattern
pierFile = (pierDir </>)

runPier :: (Display -> Rules ()) -> IO ()
runPier act = withDisplay $ \d ->
                shakeArgs shakeOptions
                            { shakeFiles = pierDir
                            , shakeProgress = progressSimple
                            , shakeChange = ChangeDigest
                            -- Detect the number of threads:
                            , shakeThreads = 0
                            }
                    $ act d

cleanAll :: Rules ()
cleanAll = action $ do
            putNormal $ "Removing " ++ pierDir
            removeFilesAfter pierDir ["//"]

data DisplayState = DisplayState !Key !(Map.Map Key Text)
    deriving Show

newtype Display = Display (MVar DisplayState)

newtype Key = Key Integer
    deriving (Show, Eq, Ord)

nextKey :: Key -> Key
nextKey (Key n) = Key (n+1)

withMessage :: Display -> Text -> Action a -> Action a
withMessage disp t act = do
    k <- liftIO $ swapState disp $ \(DisplayState k old) ->
            (DisplayState (nextKey k) $ Map.insert k t old, k)
    act `actionFinally` swapState_ disp
                (\(DisplayState k' old) -> DisplayState k' $ Map.delete k old)

withDisplay :: (Display -> IO a) -> IO a
withDisplay act = do
    d <- Display <$> newMVar empty
    act d `finally` swapState_ d (const empty)
  where
    -- TODO: this is the wrong behavior; we should preserve the most recent
    -- messages
    empty = DisplayState (Key 0) Map.empty

swapState :: Display -> (DisplayState -> (DisplayState, a)) -> IO a
swapState (Display state) f = modifyMVar state $ \old -> do
    let (new, x) = f old
    print (old, new) -- TODO
    return (new, x)

swapState_ :: Display -> (DisplayState -> DisplayState) -> IO ()
swapState_ d f = swapState d ((, ()) . f)
