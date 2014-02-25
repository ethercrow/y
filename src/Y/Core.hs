module Y.Core
    ( startCore
    ) where

import Control.Concurrent
import Control.Lens hiding (Action)
import Control.Monad
import Data.Default
import qualified FRP.Sodium as Sodium
import qualified FRP.Sodium.Internal as SodiumI (ioReactive)

import Y.CoreState
import Y.Frontend
import Y.Keymap
import Y.Buffer

startCore :: Keymap
    -> Sodium.Event InputOccurence
    -> MVar ()
    -> Sodium.Reactive (Sodium.Event ViewModel)
startCore keymap inputEvent exit = do
    (keymapBehaviour, pushKeymap) <- Sodium.newBehaviour keymap
    (stateBehaviour, pushState) <- Sodium.newBehavior (CoreState exit def)
    (outputEvent, pushOutput) <- Sodium.newEvent
    let actionEvent = Sodium.snapshot applyKeymap
                                      inputEvent
                                      keymapBehaviour
        step (PureA f) oldState = do
            let newState = f oldState
            return ( ViewModel (newState ^. buffer . text)
                   , newState
                   )
        step ExitA oldState = do
            putMVar (oldState ^. exitMVar) ()
            return ( ViewModel (oldState ^. buffer . text)
                   , oldState
                   )
        step (ImpureA f) oldState = do
            newState <- f oldState
            return ( ViewModel (newState ^. buffer . text)
                   , newState
                   )
        step (KeymapModA f) oldState = do
            Sodium.sync $ do
                currKeymap <- Sodium.sample keymapBehaviour
                pushKeymap (f currKeymap)
            return ( ViewModel (oldState ^. buffer . text)
                   , oldState
                   )
    Sodium.listen actionEvent $ \action -> void . forkIO $ do
        currentState <- Sodium.sync $ Sodium.sample stateBehaviour
        (output, newState) <- step action currentState
        Sodium.sync $ do
            pushState newState
            pushOutput output
    return outputEvent
