module Y.Keymap where

import Control.Concurrent
import qualified FRP.Sodium as Sodium
import Y.CoreState
import Y.Frontend

data Action
    = ImpureAction (CoreState -> IO CoreState)
    | PureAction (CoreState -> CoreState)
    | ExitAction

data Keymap = Keymap
    { fromKeymap :: Sodium.Event InputOccurence
                    -> Sodium.Reactive (Sodium.Event Action)
    }
