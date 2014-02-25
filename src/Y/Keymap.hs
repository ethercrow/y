module Y.Keymap where

import Data.Foldable (asum)
import qualified FRP.Sodium as Sodium
import Y.CoreState
import Y.Frontend
import Y.String
import Y.MatchResult

data Action
    = ImpureA (CoreState -> IO CoreState)
    | PureA (CoreState -> CoreState)
    | KeymapModA (Keymap -> Keymap)
    | ExitA

data KeymapState = KeymapState

type EventString = YiString

type KeymapBinding = (InputOccurence -> MatchResult Action)

data Keymap = Keymap
    { kmBindings :: [KeymapBinding]
    , kmState :: KeymapState
    }

selectBinding :: InputOccurence -> [KeymapBinding] -> MatchResult Action
selectBinding input = asum . fmap ($ input)

applyKeymap :: InputOccurence -> Keymap -> Action
applyKeymap i (Keymap bindings _) =
    case selectBinding i bindings of
        WholeMatch action -> action
        _ -> PureA id
applyKeymap _ _ = PureA id

fromKeymap :: Keymap
    -> Sodium.Event InputOccurence
    -> Sodium.Reactive (Sodium.Event Action)
fromKeymap = undefined
