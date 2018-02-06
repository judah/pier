module Pier.Core.Directory
    ( createParentIfMissing
    , renameOrCopyFile
    ) where

import Control.Exception (catchJust)
import Control.Monad (guard)
import Control.Monad.IO.Class
import Development.Shake.FilePath
import Foreign.C.Error (Errno(..), eXDEV)
import GHC.IO.Exception (ioe_errno)
import System.Directory

-- | Create recursively the parent of the given path, if it doesn't exist.
createParentIfMissing :: MonadIO m => FilePath -> m ()
createParentIfMissing path
    = liftIO $ createDirectoryIfMissing True (takeDirectory path)

renameOrCopyFile :: FilePath -> FilePath -> IO ()
renameOrCopyFile src dest =
    catchJust (\e -> ioe_errno e >>= guard . (== eXDEV) . Errno)
        (renameFile src dest)
        $ const $ copyFile src dest >> removeFile src
