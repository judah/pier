module Pier.Core.Download
    ( askDownload
    , Download(..)
    , downloadRules
    ) where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as L
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics
import Network.Wreq as Wreq
import Control.Lens ((^.))
import qualified System.Directory as Directory
import System.IO.Temp as Temp
import qualified System.IO as IO

import Pier.Core.Directory
import Pier.Core.Command
import Pier.Core.Persistent
import Pier.Core.Run

-- | Downloads @downloadUrlPrefix </> downloadName@ to
-- @downloadFilePrefix </> downloadName@.
-- Everything is stored in `~/.pier/downloads`.
data Download = Download
    { downloadUrlPrefix :: String
    , downloadName :: FilePath
    , downloadFilePrefix :: FilePath
        }
    deriving (Show, Typeable, Eq, Generic)

instance Hashable Download
instance Binary Download
instance NFData Download

type instance RuleResult Download = Artifact

askDownload :: Download -> Action Artifact
askDownload = askPersistent

-- TODO: make this its own rule type?
downloadRules :: Display -> Rules ()
downloadRules disp = addPersistent $ \d -> liftIO $ do
    -- Download to a shared location under $HOME/.pier, if it doesn't
    -- already exist (atomically); then make an artifact that symlinks to it.
    downloadsDir <- pierDownloadsDir
    let result = downloadsDir </> downloadFilePrefix d
                                        </> downloadName d
    exists <- Directory.doesFileExist result
    unless exists $ do
        k <- newKey disp
        setKeyMessage disp k $ "Downloading " ++ downloadName d
        withSystemTempFile (takeFileName $ downloadName d)
                    $ \tmp h -> do
                        IO.hClose h
                        r <- Wreq.get $ downloadUrlPrefix d </> downloadName d
                        liftIO $ L.writeFile tmp $ r ^. responseBody
                        createParentIfMissing result
                        Directory.renameFile tmp result
        removeKey disp k
    return $ externalFile result

pierDownloadsDir :: IO FilePath
pierDownloadsDir = do
    home <- Directory.getHomeDirectory
    return $ home </> ".pier/downloads"
