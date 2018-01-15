module Pier.Core.Directory
    ( createParentIfMissing
    ) where

import Control.Monad.IO.Class
import Development.Shake.FilePath
import System.Directory

-- | Create recursively the parent of the given path, if it doesn't exist.
createParentIfMissing :: MonadIO m => FilePath -> m ()
createParentIfMissing path
    = liftIO $ createDirectoryIfMissing True (takeDirectory path)
