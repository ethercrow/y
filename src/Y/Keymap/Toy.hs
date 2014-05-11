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
    where bindings =
            [ asyncChar
            , bindingThatChangesBindings
            , enterChar
            , arrow
            , anyChar
            , exit
            ]

bindingThatChangesBindings :: InputOccurrence -> MatchResult Action
bindingThatChangesBindings (KChar 'z')
    = WholeMatch (SyncA (KeymapModA (\(Keymap bs s) -> (Keymap (anyBigChar : bs) s))))
bindingThatChangesBindings _ = NoMatch

asyncChar :: InputOccurrence -> MatchResult Action
asyncChar (KChar c) | c `elem` "xy"
    = WholeMatch . AsyncA $ \_state -> do
        threadDelay 1000000
        return $! SyncA (StateModA (csBuffer . text %~ (`snoc` c)))
asyncChar _ = NoMatch

anyChar :: InputOccurrence -> MatchResult Action
anyChar (KChar c) = printCharAction c
anyChar _ = NoMatch

anyBigChar :: InputOccurrence -> MatchResult Action
anyBigChar (KChar c) = printCharAction (toUpper c)
anyBigChar _ = NoMatch

enterChar :: InputOccurrence -> MatchResult Action
enterChar KEnter = printCharAction '\n'
enterChar _ = NoMatch

arrow :: InputOccurrence -> MatchResult Action
arrow KLeft = WholeMatch (SyncA (StateModA (csBuffer . cursorPosition %~ pred)))
arrow KRight = WholeMatch (SyncA (StateModA (csBuffer . cursorPosition %~ succ)))
arrow KUp = WholeMatch (SyncA (StateModA (csBuffer %~ cursorUp)))
arrow KDown = WholeMatch (SyncA (StateModA (csBuffer %~ cursorDown)))
arrow _ = NoMatch

exit :: InputOccurrence -> MatchResult Action
exit KEsc = WholeMatch (SyncA ExitA)
exit _ = NoMatch

printCharAction :: Char -> MatchResult Action
printCharAction c
    = WholeMatch . SyncA . StateModA . over csBuffer
    $ (\(Buffer txt pos) ->
        Buffer (insertAt (singleton c) pos txt)
               (succ pos))

