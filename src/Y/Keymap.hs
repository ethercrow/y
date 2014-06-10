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

type Binding = InputOccurrence -> MatchResult Action

data Keymap = Keymap
    { fromKeymap :: Sodium.Event InputOccurrence -> Sodium.Reactive (Sodium.Event Action) }

selectBinding :: InputOccurrence -> [Binding] -> MatchResult Action
selectBinding input = asum . fmap ($ input)

mkStatelessKeymap :: [Binding] -> Keymap
mkStatelessKeymap bindings = Keymap
    $ return
    . Sodium.filterJust
    . fmap (\i -> case selectBinding i bindings of
                WholeMatch action -> Just action
                _ -> Nothing)

