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
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import qualified System.Directory as Directory
import System.IO.Temp as Temp
import qualified System.IO as IO

import Pier.Core.Directory
import Pier.Core.Command
import Pier.Core.Persistent

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
downloadRules :: Rules ()
downloadRules = do
    manager <- liftIO $ newManager tlsManagerSettings
    addPersistent $ \d -> do
    -- Download to a shared location under $HOME/.pier, if it doesn't
    -- already exist (atomically); then make an artifact that symlinks to it.
    downloadsDir <- liftIO pierDownloadsDir
    let result = downloadsDir </> downloadFilePrefix d
                                        </> downloadName d
    exists <- liftIO $ Directory.doesFileExist result
    unless exists $ do
        putNormal $ "Downloading " ++ downloadName d
        liftIO $ withSystemTempFile (takeFileName $ downloadName d)
                    $ \tmp h -> do
                        IO.hClose h
                        let url = downloadUrlPrefix d </> downloadName d
                        req <- parseRequest url
                        resp <- httpLbs req manager
                        unless (statusIsSuccessful . responseStatus $ resp)
                            $ error $ "Unable to download: " ++ show url
                                    ++ ": " ++ show (responseStatus resp)
                        liftIO . L.writeFile tmp . responseBody $ resp
                        createParentIfMissing result
                        Directory.renameFile tmp result
    return $ externalFile result

pierDownloadsDir :: IO FilePath
pierDownloadsDir = do
    home <- Directory.getHomeDirectory
    return $ home </> ".pier/downloads"
