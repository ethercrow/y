module Y.Keymap where

import Data.Foldable (asum)
import qualified FRP.Sodium as Sodium

import Y.Common
import Y.CoreState
import Y.String
import Y.MatchResult

data Action
    = AsyncA (CoreState -> IO Action)
    | SyncA SyncAction

isSync :: Action -> Bool
isSync (SyncA _) = True
isSync _ = False

data SyncAction
    = StateModA (CoreState -> CoreState)
    | KeymapModA (Keymap -> Keymap)
    | ExitA

instance Show Action where
    show (AsyncA _) = "AsyncA"
    show (SyncA sa) = show sa

instance Show SyncAction where
    show (StateModA _) = "StateModA"
    show (KeymapModA _) = "KeymapModA"
    show ExitA = "ExitA"

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
        _ -> SyncA (StateModA id)

fromKeymap :: Keymap
    -> Sodium.Event InputOccurrence
    -> Sodium.Reactive (Sodium.Event Action)
fromKeymap = undefined
