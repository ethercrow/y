{-# LANGUAGE RecursiveDo #-}
module Y.Core
    ( startCore
    , CoreOutput(..)
    ) where

import Control.Lens hiding (Action)
import Data.Default
import qualified FRP.Sodium as Sodium
import qualified FRP.Sodium.IO as SodiumIO

import Y.Buffer
import Y.Common
import Y.Config
import Y.CoreState
import Y.Keymap

startCore :: Config
    -> Sodium.Event InputOccurrence
    -> Sodium.Reactive (Sodium.Event CoreOutput, IO ())
startCore config inputEvent = do
    rec
        configBehaviour <- Sodium.accum config configModEvent
        stateBehaviour <- Sodium.accum (CoreState def) stateModEvent

        let actionEvent = Sodium.merge
                            (Sodium.snapshot (\i conf -> applyKeymap i (conf ^. cfgKeymap))
                                            inputEvent
                                            configBehaviour)
                            asyncActionEvent

            syncActionEvent = actionEvent
                            & Sodium.filterE isSync
                            & fmap (\(SyncA x) -> x)

            asyncActionEvent = actionEvent
                             & Sodium.filterE (not . isSync)
                             & fmap (\(AsyncA x) -> do
                                        currentState <- Sodium.sync $ Sodium.sample stateBehaviour
                                        x currentState)
                             & SodiumIO.executeAsyncIO

            outputEvent = Sodium.merge
                (fmap (OutputViewModel . ViewModel . view (buffer . text))
                    (Sodium.value stateBehaviour))
                (fmap (const OutputExit)
                    (Sodium.filterE isExit syncActionEvent))

            configModEvent = Sodium.filterJust
                           $ fmap (\action -> case action of
                                        KeymapModA f -> Just (over cfgKeymap f)
                                        _ -> Nothing)
                                  syncActionEvent

            stateModEvent = Sodium.filterJust
                          $ fmap (\action -> case action of
                                        StateModA f -> Just f
                                        _ -> Nothing)
                                 syncActionEvent

    return (outputEvent, return ())

isExit :: SyncAction -> Bool
isExit ExitA = True
isExit _ = False
