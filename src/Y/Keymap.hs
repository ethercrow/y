module Y.Keymap where

import Data.Foldable (asum)
import qualified FRP.Sodium as Sodium
import Y.Common
import Y.CoreState
import Y.String
import Y.MatchResult

data Action
    = ImpureA (CoreState -> IO CoreState)
    | PureA (CoreState -> CoreState)
    | KeymapModA (Keymap -> Keymap)
    | AsyncA (CoreState -> IO Action)
    | ExitA

data KeymapState = KeymapState

type EventString = YiString

type KeymapBinding = (InputOccurrence -> MatchResult Action)

data Keymap = Keymap
    { kmBindings :: [KeymapBinding]
    , kmState :: KeymapState
    }

selectBinding :: InputOccurrence -> [KeymapBinding] -> MatchResult Action
selectBinding input = asum . fmap ($ input)

applyKeymap :: InputOccurrence -> Keymap -> Action
applyKeymap i (Keymap bindings _) =
    case selectBinding i bindings of
        WholeMatch action -> action
        _ -> PureA id

fromKeymap :: Keymap
    -> Sodium.Event InputOccurrence
    -> Sodium.Reactive (Sodium.Event Action)
fromKeymap = undefined
