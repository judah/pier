{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake.Persistent
    ( addPersistent
    , askPersistent
    , askPersistents
    ) where

import Data.Binary (encode, decodeOrFail)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule

newtype PersistentQ question = PersistentQ question
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

newtype PersistentA answer = PersistentA { unPersistentA :: answer }
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult (PersistentQ q) = PersistentA (RuleResult q)

addPersistent
    :: (RuleResult q ~ a, ShakeValue q, ShakeValue a)
    => (q -> Action a)
    -> Rules ()
addPersistent act = addBuiltinRule noLint $ \(PersistentQ q) old depsChanged
                    -> case old of
    Just old' | not depsChanged
              , Just val <- decode' old'
                    -> return $ RunResult ChangedNothing old' val
    _ -> do
            new <- PersistentA <$> act q
            return $ RunResult
                    (if (old >>= decode') == Just new
                        then ChangedRecomputeSame
                        else ChangedRecomputeDiff)
                    (encode' new)
                    new
    where
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
    PersistentA answer <- apply1 $ PersistentQ question
    return answer

askPersistents
    :: (RuleResult q ~ a, ShakeValue q, ShakeValue a)
    => [q]
    -> Action [a]
askPersistents = fmap (map unPersistentA) . apply . map PersistentQ
