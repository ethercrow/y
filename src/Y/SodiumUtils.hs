{-# LANGUAGE MonadComprehensions #-}
module Y.SodiumUtils where

import qualified FRP.Sodium as Sodium

uniqueUpdates :: Eq a => Sodium.Behavior a -> Sodium.Reactive (Sodium.Event a)
uniqueUpdates behavior = do
    initial <- Sodium.sample behavior
    let possiblyDuplicateValues = Sodium.updates behavior
    uniqueValues <- Sodium.collectE (\new old -> ([new | new /= old], new))
                                    initial
                                    possiblyDuplicateValues
    return $! Sodium.filterJust uniqueValues