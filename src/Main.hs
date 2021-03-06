{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Lens
import Data.Maybe
import qualified Data.Text.Lazy.IO as TIO
import qualified FRP.Sodium as Sodium
import System.Environment

import Y.Buffer
import Y.Config
import Y.Core
import Y.CoreState
import Y.Keymap
import Y.Frontend
import Y.Mode.Diff
import Y.Mode.Erlang
import Y.Mode.Fundamental
import qualified Y.String as S

import Y.Frontend.Toy
import Y.Frontend.Vty

import Y.Keymap.Toy
import Y.Keymap.Emacs

main :: IO ()
main = do
    maybeFilename <- listToMaybe <$> getArgs
    fe <- startVtyFrontend
    -- fe <- startToyFrontend

    let config = Config toyKeymap [erlangMode, fundamentalMode, diffMode]
        inputEvent = fe ^. feInputEvent

    startActions <- case maybeFilename of
        Just filename -> do
            content <- TIO.readFile filename
            let showTextAction = SyncA (StateModA (csBuffer . text .~ S.fromLazyText content))
            return [showTextAction]
        Nothing -> return []

    (viewModels, shutdown) <- Sodium.sync $ startCore config inputEvent startActions

    putStrLn "Proceeding to frontend's main loop"
    (fe ^. feMainLoop) viewModels
    shutdown
