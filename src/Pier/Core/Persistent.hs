{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
module Pier.Core.Persistent
    ( addPersistent
    , askPersistent
    , askPersistents
    , watchingGit
    , gitRev
    ) where


import Data.Binary (encode, decodeOrFail)
import Development.GitRev (gitHash, gitDirtyTracked)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule
import Control.Monad (void)
import GHC.Generics
import Language.Haskell.TH (ExpQ)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

newtype Persistent question = Persistent question
    deriving (Typeable, Eq, Generic, Hashable, Binary, NFData)

-- Improve error messages by just forwarding the instance of the
-- wrapped type.
instance Show q => Show (Persistent q) where
    show (Persistent q) = show q

newtype PersistentA answer = PersistentA { unPersistentA :: answer }
    deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type instance RuleResult (Persistent q) = PersistentA (RuleResult q)

addPersistent
    :: (RuleResult q ~ a, ShakeValue q, ShakeValue a)
    => (q -> Action a)
    -> Rules ()
addPersistent act = addBuiltinRule noLint $ \(Persistent q) old depsChanged
                    -> case old of
    Just old' | not depsChanged
              , Just val <- decode' old'
                    -> return $ RunResult ChangedNothing old' val
    _ -> do
            rerunIfGitChanged
            new <- PersistentA <$> act q
            return $ RunResult
                    (if (old >>= decode') == Just new
                        then ChangedRecomputeSame
                        else ChangedRecomputeDiff)
                    (encode' new)
                    new

encode' :: Binary a => a -> BS.ByteString
encode' = BS.concat . LBS.toChunks . encode

decode' :: Binary a => BS.ByteString -> Maybe a
decode' b = case decodeOrFail $ LBS.fromChunks [b] of
                Right (bs,_,x)
                    | LBS.null bs -> Just x
                _ -> Nothing


askPersistent
    :: (RuleResult q ~ a, ShakeValue q, ShakeValue a)
    => q
    -> Action a
askPersistent question = do
    PersistentA answer <- apply1 $ Persistent question
    return answer

askPersistents
    :: (RuleResult q ~ a, ShakeValue q, ShakeValue a)
    => [q]
    -> Action [a]
askPersistents = fmap (map unPersistentA) . apply . map Persistent

-----------
-- Use the git revision to decide whether any code has changed, which would
-- indicate that we should recompute the Persistent cache.
-- Since `stack.yaml` is also tracked in git, this logic also covers
-- version changes of any third-party dependencies.
--
-- If the git status for any tracked file is dirty, we always recompute
-- everything.
--
-- (We could make development of Pier a little nicer if we cached the output
-- of "git diff HEAD"; however gitrev doesn't provide that directly.)

data GitRev = GitRevDirty | GitRevHash String
    deriving (Show, Typeable, Eq, Generic, Binary, NFData, Hashable)

makeGitRev :: Bool -> String -> GitRev
makeGitRev True = const GitRevDirty
makeGitRev False = GitRevHash

-- Wait to actually compute the GitRev until the Main file.  Staging restrictions
-- prevent using it directly in this file.  Additionally, since gitrev calls
-- `addDependentFile` on files under `.git`, I believe that would make GHC
-- rebuilds too aggressive.
gitRev :: ExpQ
gitRev = [|makeGitRev $(gitDirtyTracked) $(gitHash)|]

rerunIfGitChanged :: Action ()
rerunIfGitChanged = void $ apply1 GitChanged

watchingGit :: GitRev -> Rules ()
watchingGit rev = do
    action rerunIfGitChanged
    addBuiltinRule noLint $ \GitChanged maybe_enc _ ->
            let changed = case rev of
                            GitRevDirty -> ChangedRecomputeDiff
                            GitRevHash newHash
                                | Just enc <- maybe_enc
                                , Just (GitRevHash oldHash) <- decode' enc
                                , oldHash == newHash -> ChangedNothing
                                | otherwise -> ChangedRecomputeDiff
            in return $ RunResult changed (encode' rev) rev

data GitChanged = GitChanged
    deriving (Show, Typeable, Eq, Generic, Binary, NFData, Hashable)
type instance RuleResult GitChanged = GitRev
