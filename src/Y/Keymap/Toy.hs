{-# LANGUAGE LambdaCase #-}

module Y.Keymap.Toy where

import Control.Lens hiding (cons, snoc, Action)
import Data.Char (toUpper)
import Y.Buffer
import Y.CoreState
import Y.Frontend
import Y.Keymap
import Y.String
import Y.MatchResult

toyKeymap :: Keymap
toyKeymap = Keymap bindings KeymapState
    where bindings = [bindingThatChangesBindings, anyChar, exit]

bindingThatChangesBindings :: InputOccurence -> MatchResult Action
bindingThatChangesBindings (KChar 'z')
    = WholeMatch (KeymapModA (\(Keymap bs s) -> (Keymap (anyBigChar : bs) s)))
bindingThatChangesBindings _ = NoMatch

anyChar :: InputOccurence -> MatchResult Action
anyChar (KChar c) = WholeMatch (PureA (buffer . text %~ (`snoc` c)))
anyChar _ = NoMatch

anyBigChar :: InputOccurence -> MatchResult Action
anyBigChar (KChar c) = WholeMatch (PureA (buffer . text %~ (`snoc` toUpper c)))
anyBigChar _ = NoMatch

exit :: InputOccurence -> MatchResult Action
exit KEsc = WholeMatch ExitA
exit _ = NoMatch
