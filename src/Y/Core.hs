module Y.Core
    ( startCore
    ) where

import Control.Concurrent
import Control.Lens
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
startCore keymap input exit
    = do
        actionEvent <- fromKeymap keymap input
        collectE' go (CoreState False exit def) actionEvent
    where go (PureAction f) oldState = do
              let newState = f oldState
              return ( ViewModel (newState ^. buffer . text)
                     , newState
                     )
          go ExitAction oldState = do
              putMVar (oldState ^. exitMVar) ()
              return ( ViewModel (oldState ^. buffer . text)
                     , oldState
                     )
          go (ImpureAction f) oldState = do
              newState <- f oldState
              return ( ViewModel (newState ^. buffer . text)
                     , newState
                     )

-- collectE' :: Context r
--     => (a -> s -> IO (b, s))
--     -> s
--     -> Sodium.Event r a
--     -> Sodium.Reactive r (Sodium.Event r b)
collectE' step initial inputEvent = do
    (stateBeh, pushState) <- Sodium.newBehavior initial
    (outputEvent, pushOutput) <- Sodium.newEvent
    Sodium.listen inputEvent $ \input -> void . forkIO $ do
        currentState <- Sodium.sync $ Sodium.sample stateBeh
        (output, newState) <- step input currentState
        Sodium.sync $ do
            pushState newState
            pushOutput output
    return outputEvent
