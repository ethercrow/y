{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Y.Core
    ( startCore
    ) where

import Control.Applicative
import Control.Lens hiding (Action)
import Data.Default
import Data.Monoid
import qualified FRP.Sodium as Sodium
import qualified FRP.Sodium.IO as SodiumIO

import Y.Common
import Y.Config
import Y.CoreState
import Y.Keymap
import Y.Mode
import Y.Output

startCore :: Config
    -> Sodium.Event InputOccurrence
    -> [Action]
    -> Sodium.Reactive (Sodium.Event CoreOutput, IO ())
startCore config inputEvent startupActions = do
    rec
        let bufferBehavior = fmap (view csBuffer) stateBehavior
            modeBehavior = head . view cfgModes <$> configBehavior
            modeOutputBehaviorAction :: Sodium.Behavior (Sodium.Reactive ModeOutput)
            modeOutputBehaviorAction = modeBehavior <*> pure bufferBehavior
            modeOutputEvent :: Sodium.Event ModeOutput
            modeOutputEvent = Sodium.execute $ Sodium.value modeOutputBehaviorAction
            modeActionEvent :: Sodium.Event Action
        modeActionEvent <- Sodium.switchE <$> Sodium.hold mempty (fmap (view moActionEvent) modeOutputEvent)

        configBehavior <- Sodium.accum config configModEvent
        stateBehavior <- Sodium.accum (CoreState def def (80, 25)) stateModEvent
        (startupActionEvent, pushStartupAction) <- Sodium.newEvent

        let userActionEventFromDynamicKeymap :: Sodium.Event (Sodium.Event Action)
            userActionEventFromDynamicKeymap =
                Sodium.execute (Sodium.value (fromKeymap <$> keymapBehavior <*> pure inputEvent))
        userActionEventFromInitialKeymap <-
            let Keymap k = config ^. cfgKeymap
            in k inputEvent
        userActionEvent <- Sodium.switchE
                        <$> Sodium.hold userActionEventFromInitialKeymap
                                        userActionEventFromDynamicKeymap

        let keymapBehavior = fmap (view cfgKeymap) configBehavior
            actionEvent = mconcat
                [ startupActionEvent
                , userActionEvent
                , asyncActionEvent
                , modeActionEvent
                ]

            syncActionEvent = pickSync actionEvent

            asyncActionEvent = actionEvent
                             & Sodium.filterE (not . isSync)
                             & fmap (\(AsyncA x) -> do
                                        currentState <- Sodium.sync $ Sodium.sample stateBehavior
                                        x currentState)
                             & SodiumIO.executeAsyncIO

            showableOutputEvent = makeOutput <$> Sodium.value stateBehavior

            outputEvent = showableOutputEvent <> (OutputExit <$ Sodium.filterE isExit syncActionEvent)
            configModEvent = pickConfigMod syncActionEvent
            stateModEvent = pickStateMod syncActionEvent

    mapM_ pushStartupAction startupActions

    return (outputEvent, return ())

isExit :: SyncAction -> Bool
isExit ExitA = True
isExit _ = False

-- TODO: Is there a way to generalize these functions?
pickConfigMod :: Sodium.Event SyncAction -> Sodium.Event (Config -> Config)
pickConfigMod
    = Sodium.filterJust
    . fmap (\action -> case action of
        KeymapModA f -> Just (over cfgKeymap f)
        _ -> Nothing)

pickStateMod :: Sodium.Event SyncAction -> Sodium.Event (CoreState -> CoreState)
pickStateMod
    = Sodium.filterJust
    . fmap (\action -> case action of
        StateModA f -> Just f
        _ -> Nothing)

pickSync :: Sodium.Event Action -> Sodium.Event SyncAction
pickSync = fmap (\(SyncA x) -> x) . Sodium.filterE isSync
