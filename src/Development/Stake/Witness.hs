{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Development.Stake.Witness where

import Data.Binary (encode, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule

newtype WitnessQ question = WitnessQ question
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

newtype WitnessA answer = WitnessA { unWitnessA :: answer }
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult (WitnessQ q) = WitnessA (RuleResult q)

addWitness
    :: (RuleResult q ~ a, ShakeValue q, ShakeValue a)
    => (q -> Action a)
    -> Rules ()
addWitness act = addBuiltinRule noLint $ \(WitnessQ q) old depsChanged
                    -> case old of
    Just old' | not depsChanged -> return $ RunResult ChangedNothing old'
                                         $ decode' old'
    _ -> do
            new <- WitnessA <$> act q
            return $ RunResult
                    (if fmap decode' old == Just new
                        then ChangedRecomputeSame
                        else ChangedRecomputeDiff)
                    (encode' new)
                    new
    where
        encode' :: Binary a => a -> BS.ByteString
        encode' = BS.concat . LBS.toChunks . encode

        decode' :: Binary a => BS.ByteString -> a
        decode' = decode . LBS.fromChunks . return


askWitness
    :: (RuleResult q ~ a, ShakeValue q, ShakeValue a)
    => q
    -> Action a
askWitness question = do
    WitnessA answer <- apply1 $ WitnessQ question
    return answer

askWitnesses
    :: (RuleResult q ~ a, ShakeValue q, ShakeValue a)
    => [q]
    -> Action [a]
askWitnesses = fmap (map unWitnessA) . apply . map WitnessQ
