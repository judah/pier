{-# LANGUAGE DeriveAnyClass #-}
module Development.Stake.Core
    ( -- * Build directory
      runStake
    , artifact
    , (#>)
      -- * Cleaning
    , rerunIfCleaned
    , cleanBuild
    , cleanAll
      -- * Directory utilities
    , createParentIfMissing

    ) where

import Control.Monad.IO.Class
import Development.Shake
import Development.Shake.FilePath
import System.Directory

stakeDir :: FilePath
stakeDir = ".stake"

-- TODO: newtype describing inputs/outputs:
artifact :: FilePattern -> FilePattern
artifact = (stakeDir </>)

(#>) :: FilePath -> (FilePath -> [String] -> Action ()) -> Rules ()
pat #> act = pat' %> \f -> case filePattern pat' f of
                                Just ms -> act f ms
                                Nothing -> fail $ "Shouldn't happen: no match"
                                                ++ " for pattern " ++ show (pat', f)
  where
    pat' = artifact pat
infixl #>

runStake :: ([String] -> Rules ()) -> IO ()
runStake rules = shakeArgsWith shakeOptions
                        { shakeFiles = stakeDir
                        , shakeProgress = progressSimple
                        , shakeVerbosity = Chatty
                        } [] $ \[] args -> return $ Just $ cleaner >> rules args

runClean :: FilePattern -> Rules ()
runClean pat = action $ removeFilesAfter stakeDir [pat]

cleanBuild, cleanAll :: Rules ()
cleanBuild = runClean "build"
cleanAll = runClean ""

createParentIfMissing :: MonadIO m => FilePath -> m ()
createParentIfMissing path
    = liftIO $ createDirectoryIfMissing True (takeDirectory path)


rerunIfCleaned :: Action ()
rerunIfCleaned = need [cleanFile]

cleanFile :: FilePath
cleanFile = artifact "build/sentinel"

cleaner :: Rules ()
cleaner = cleanFile %> \f -> do
    Stdout c <- cmd "uuidgen"
    writeFile' f c


{-
data Cleaner = Cleaner
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult Cleaner = Bool

setCleaned :: Bool -> Rules ()
setCleaned c = 
-}
