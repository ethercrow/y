module Y.Core
    ( startCore
    , CoreOutput(..)
    ) where

import Control.Concurrent
import Control.Lens hiding (Action)
import Control.Monad
import Data.Default
import qualified FRP.Sodium as Sodium
import qualified FRP.Sodium.Internal as SodiumI

import Y.Buffer
import Y.Config
import Y.CoreState
import Y.Frontend
import Y.Keymap

data CoreOutput
    = OutputViewModel ViewModel
    | OutputNoop
    | OutputExit

startCore :: Config
    -> Sodium.Event InputOccurrence
    -> Sodium.Reactive (Sodium.Event CoreOutput)
startCore config inputEvent = do
    (configBehaviour, pushConfig) <- Sodium.newBehaviour config
    (stateBehaviour, pushState) <- Sodium.newBehavior (CoreState def)
    (outputEvent, pushOutput) <- Sodium.newEvent
    (actionEvent, pushAction) <- Sodium.newEvent
    _ <- SodiumI.listenTrans (Sodium.snapshot (\i conf -> applyKeymap i (conf ^. cfgKeymap))
                                              inputEvent
                                              configBehaviour)
                             pushAction
    let step (PureA f) oldState = do
            let newState = f oldState
            return ( OutputViewModel $ ViewModel (newState ^. buffer . text)
                   , newState
                   )
        step ExitA oldState = return (OutputExit, oldState)
        step (ImpureA f) oldState = do
            newState <- f oldState
            return ( OutputViewModel $ ViewModel (newState ^. buffer . text)
                   , newState
                   )
        step (KeymapModA f) oldState = do
            Sodium.sync $ do
                currConfig <- Sodium.sample configBehaviour
                pushConfig (currConfig & cfgKeymap %~ f)
            return ( OutputViewModel $ ViewModel (oldState ^. buffer . text)
                   , oldState
                   )
        step (AsyncA action) oldState = do
            _ <- forkIO $ action oldState >>= (Sodium.sync . pushAction)
            return ( OutputNoop
                   , oldState
                   )
    _ <- Sodium.listen actionEvent $ \action -> void . forkIO $ do
        currentState <- Sodium.sync $ Sodium.sample stateBehaviour
        (output, newState) <- step action currentState
        Sodium.sync $ do
            pushState newState
            pushOutput output
    return outputEvent
