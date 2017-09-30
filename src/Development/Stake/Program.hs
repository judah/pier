{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake.Program where

import Control.Exception (Exception, throw)
import Control.Monad.IO.Class (liftIO)
import Development.Shake
import Development.Shake.Classes
import System.Directory (findExecutable)

-- TODO: try a class-based API

-- TODO: depend on the program itself?  So that if it changes
-- (or points to a different path), we trigger a recompilation?

newtype Program = Program { programName :: String }
        deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult Program = FilePath

data ProgramNotFound = ProgramNotFound String
    deriving Show
instance Exception ProgramNotFound

addProgramOracle :: Rules (Program -> Action FilePath)
addProgramOracle = addOracle $ \(Program s) -> do
   liftIO (findExecutable s) >>= \case
        Nothing -> liftIO $ throw $ ProgramNotFound s
        Just path -> return path

ghc, ghcPkg :: Program
ghc = Program "ghc"
ghcPkg = Program "ghc-pkg"
