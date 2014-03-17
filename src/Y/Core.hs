module Y.Core
    ( startCore
    , CoreOutput(..)
    ) where

import Control.Applicative
import Control.Lens hiding (Action)
import Data.Default
import qualified FRP.Sodium as Sodium
import qualified FRP.Sodium.Internal as SodiumI

import Y.Buffer
import Y.Common
import Y.Config
import Y.CoreState
import Y.Keymap

import Debug.Trace

startCore :: Config
    -> Sodium.Event InputOccurrence
    -> Sodium.Reactive (Sodium.Event CoreOutput, IO ())
startCore config inputEvent = do
    (configBehaviour, pushConfig) <- Sodium.newBehaviour config
    (stateBehaviour, pushState) <- Sodium.newBehavior (CoreState def)
    (outputEvent, pushOutput) <- Sodium.newEvent

    -- (additionalActionEvent, pushAction) <- Sodium.newEvent

    let actionEvent = Sodium.snapshot (\i conf -> applyKeymap i (conf ^. cfgKeymap))
                                          inputEvent
                                          configBehaviour

        syncActionEvent = actionEvent
                        & Sodium.filterE isSync
                        & fmap (\(SyncA x) -> x)

    _ <- Sodium.listen syncActionEvent (putStrLn . ("debugSA: " ++) . show)
    _ <- Sodium.listen (Sodium.values stateBehaviour)
                      (putStrLn . ("debugState: " ++) . show . (view (buffer . text)))

    unlisten <- SodiumI.listenTrans syncActionEvent $ \action -> case trace (show action) action of
            StateModA f -> do
                newState <- f <$> Sodium.sample stateBehaviour
                pushState $! trace "changing state" newState
                pushOutput (OutputViewModel $ ViewModel (newState ^. buffer . text))
            KeymapModA f -> do
                currentConfig <- Sodium.sample configBehaviour
                pushConfig $! trace "changing config" $ currentConfig & cfgKeymap %~ f
            ExitA -> pushOutput OutputExit

    return (outputEvent, unlisten)
