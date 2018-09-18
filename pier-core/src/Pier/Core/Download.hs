module Pier.Core.Download
    ( askDownload
    , Download(..)
    , downloadRules
    ) where

import Control.Monad (unless)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Pier.Core.Artifact
import Pier.Core.Internal.Directory
import Pier.Core.Internal.Store
import Pier.Core.Persistent

-- | Downloads @downloadUrlPrefix / downloadName@ to
-- @downloadFilePrefix / downloadName@.
-- Everything is stored in `~/.pier/downloads`.
data Download = Download
    { downloadUrlPrefix :: String
    , downloadName :: FilePath
    }
    deriving (Typeable, Eq, Generic)

instance Show Download where
    show d = "Download " ++ show (downloadName d)
            ++ " from " ++ show (downloadUrlPrefix d)

instance Hashable Download
instance Binary Download
instance NFData Download

type instance RuleResult Download = Artifact

askDownload :: Download -> Action Artifact
askDownload = askPersistent

downloadRules :: Maybe SharedCache -> Rules ()
downloadRules sharedCache = do
    manager <- liftIO $ newManager tlsManagerSettings
    addPersistent $ \d -> do
    h <- makeHash . T.encodeUtf8 . T.pack
            $ "download: " ++ show d
    let name = downloadName d
    let msg = "Downloading " ++ name
    createArtifacts sharedCache h [msg] $ \tmpDir -> do
        let out = tmpDir </> name
        createParentIfMissing out
        liftIO $ do
            let url = downloadUrlPrefix d ++ "/" ++ downloadName d
            req <- parseRequest url
            resp <- httpLbs req manager
            unless (statusIsSuccessful . responseStatus $ resp)
                $ error $ "Unable to download " ++ show url
                        ++ "\nStatus: " ++ showStatus (responseStatus resp)
            liftIO . L.writeFile out . responseBody $ resp
    return $ builtArtifact h name
  where
    showStatus s = show (statusCode s) ++ " " ++ BC.unpack (statusMessage s)
