module Pier.Core.Run
    ( -- * Build directory
      runPier
    , pierFile
    , cleanAll
      -- * Display
    , Display
    , Key
    , newKey
    , setKeyMessage
    , removeKey
    ) where

import Data.Bifunctor (second)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (forever)
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Development.Shake
import Development.Shake.FilePath
import System.Console.Terminfo

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

-- TODO: protect against display being too short of width
-- TODO: move to another module
-- TODO: don't stop indicator at the end... 
-- TODO: protect against terminal without movements
-- TODO: protect against no terminal (and allow explicit opt-out or opt-in)
data DisplayState = DisplayState
    { _displayKey :: !Key
    , displayMessages :: !(Map.Map Key (Text, Indicator))
    }

newtype Indicator = Indicator Int
    deriving (Show, Eq)

data Display = Display (MVar DisplayState) Term

data Term = Term
    { termTerminal :: Terminal
    , termMoveUp :: TermOutput
    , termClearLine :: TermOutput
    , termNewLine :: TermOutput
    }

newTerm :: IO Term
newTerm = do
    t <- setupTermFromEnv
    -- TODO: catch better
    let Just cr = getCapability t carriageReturn
    let Just clear = getCapability t clearEOL
    let Just u = getCapability t moveUp
    let Just nl = getCapability t newline
    return $ Term
        { termTerminal = t
        , termClearLine = cr <#> clear
        , termMoveUp = u 1
        , termNewLine = nl
        }

newtype Key = Key Integer
    deriving (Show, Eq, Ord)

nextKey :: Key -> Key
nextKey (Key n) = Key (n+1)

newKey :: Display -> IO Key
newKey disp = swapState disp $ \(DisplayState k old) ->
            (DisplayState (nextKey k) old, k)

-- TODO: take Text?
setKeyMessage :: Display -> Key -> String -> IO ()
setKeyMessage disp k msg = swapState_ disp
    $ \(DisplayState k' old)
        -> DisplayState k' $ Map.insert k (pack msg, Indicator 0) old

removeKey :: Display -> Key -> IO ()
removeKey disp k = swapState_ disp
                    $ \(DisplayState k' old) -> DisplayState k' $ Map.delete k old

withDisplay :: (Display -> IO a) -> IO a
withDisplay act = do
    d <- Display <$> newMVar empty <*> newTerm
    a <- async $ forever $ pokeDisplay d >> threadDelay 1000000
    act d `finally` (swapState_ d (const empty) >> cancel a)
  where
    -- TODO: this is the wrong behavior; we should preserve the most recent
    -- messages
    empty = DisplayState (Key 0) Map.empty
    pokeDisplay d = swapState_ d $ \(DisplayState k old)
            -> DisplayState k $ fmap (second nextIndicator) old

swapState :: Display -> (DisplayState -> (DisplayState, a)) -> IO a
swapState (Display state term) f = modifyMVar state $ \old -> do
    let (new, x) = f old
    let oldMsgs = displayMessages old
    let newMsgs = displayMessages new
    showDiff term oldMsgs newMsgs
    return (new, x)

swapState_ :: Display -> (DisplayState -> DisplayState) -> IO ()
swapState_ d f = swapState d ((, ()) . f)

showDiff :: Term -> Map Key (Text, Indicator) -> Map Key (Text, Indicator) -> IO ()
showDiff term old new
    | old == new = return ()
    | otherwise =
    runTermOutput (termTerminal term)
        $ mreplicate (Map.size old) (termMoveUp term <#> termClearLine term)
        <> foldMap (\(t, i) -> termText (unpack t ++ showIndicator i) <> termNewLine term)
            (Map.elems new)

mreplicate :: Monoid m => Int -> m -> m
mreplicate n m
    | n <= 0    = mempty
    | otherwise = m `mappend` mreplicate (n-1) m

showIndicator :: Indicator -> String
showIndicator (Indicator i) = replicate i '.'

nextIndicator :: Indicator -> Indicator
nextIndicator (Indicator n)
    | n >= 10 = Indicator n
    | otherwise = Indicator $ n + 1
