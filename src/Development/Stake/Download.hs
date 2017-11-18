module Development.Stake.Download
    ( askDownload
    , Download(..)
    , downloadRules
    ) where

import qualified Data.ByteString.Lazy as L
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Stake.Core
import Development.Stake.Witness
import GHC.Generics
import Network.Wreq as Wreq
import Control.Lens ((^.))

-- | Downloads @downloadUrlPrefix </> downloadName@ to
-- @downloadFilePrefix </> downloadName@.
data Download = Download
    { downloadUrlPrefix :: String
    , downloadName :: FilePath
    , downloadFilePrefix :: FilePath
        }
    deriving (Show, Typeable, Eq, Generic)

instance Hashable Download
instance Binary Download
instance NFData Download

type instance RuleResult Download = FilePath

askDownload :: Download -> Action FilePath
askDownload = askWitness

downloadRules :: Rules ()
downloadRules = addWitness $ \d -> do
    putNormal $ "Downloading " ++ downloadName d
    r <- liftIO $ Wreq.get $ downloadUrlPrefix d </> downloadName d
    let result = stakeFile $ "downloads" </> downloadFilePrefix d
                                        </> downloadName d
    createParentIfMissing result
    liftIO $ L.writeFile result $ r ^. responseBody
    return result
