module Pier.Core.Directory
    ( createParentIfMissing
    , createSystemTempDirectory
    ) where

import Control.Monad.IO.Class
import Development.Shake.FilePath
import System.Directory
import System.IO.Temp

-- | Create recursively the parent of the given path, if it doesn't exist.
createParentIfMissing :: MonadIO m => FilePath -> m ()
createParentIfMissing path
    = liftIO $ createDirectoryIfMissing True (takeDirectory path)

createSystemTempDirectory :: MonadIO m => String -> m FilePath
createSystemTempDirectory name =
    liftIO $ getCanonicalTemporaryDirectory
                >>= flip createTempDirectory name
