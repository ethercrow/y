{-# LANGUAGE LambdaCase #-}

module Y.Keymap.Toy where

import Control.Concurrent (threadDelay)
import Control.Lens hiding (cons, snoc, Action)
import Data.Char (toUpper)
import Y.Buffer
import Y.Common
import Y.CoreState
import Y.Keymap
import Y.String
import Y.MatchResult

toyKeymap :: Keymap
toyKeymap = Keymap bindings KeymapState
    where bindings = [asyncChar, bindingThatChangesBindings, anyChar, exit]

bindingThatChangesBindings :: InputOccurrence -> MatchResult Action
bindingThatChangesBindings (KChar 'z')
    = WholeMatch (KeymapModA (\(Keymap bs s) -> (Keymap (anyBigChar : bs) s)))
bindingThatChangesBindings _ = NoMatch

asyncChar :: InputOccurrence -> MatchResult Action
asyncChar (KChar c) | c `elem` "xy"
    = WholeMatch . AsyncA $ \_state -> do
        threadDelay 1000000
        return $! PureA (buffer . text %~ (`snoc` c))
asyncChar _ = NoMatch

anyChar :: InputOccurrence -> MatchResult Action
anyChar (KChar c) = WholeMatch (PureA (buffer . text %~ (`snoc` c)))
anyChar _ = NoMatch

anyBigChar :: InputOccurrence -> MatchResult Action
anyBigChar (KChar c) = WholeMatch (PureA (buffer . text %~ (`snoc` toUpper c)))
anyBigChar _ = NoMatch

exit :: InputOccurrence -> MatchResult Action
exit KEsc = WholeMatch ExitA
exit _ = NoMatch
