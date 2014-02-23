{-# LANGUAGE LambdaCase #-}

module Y.Keymap.Toy where

import Control.Lens hiding (cons, snoc)
import Control.Concurrent
import Y.Buffer
import Y.CoreState
import Y.Frontend
import Y.Keymap
import Y.String

toyKeymap :: Keymap
toyKeymap = Keymap $ \eInput -> return $
    fmap (\i -> case i of
            KEsc -> ExitAction
            KChar c -> PureAction (buffer . text %~ (`snoc` c)))
         eInput
