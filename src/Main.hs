{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens
import qualified FRP.Sodium as Sodium

import Y.Core
import Y.Frontend
import Y.Config
import Y.Highlighter.Diff

import Y.Frontend.Toy
import Y.Frontend.Vty

import Y.Keymap.Toy

main :: IO ()
main = do
    putStrLn "Started"

    -- fe <- startToyFrontend
    fe <- startVtyFrontend

    let config = Config toyKeymap diffHighlighter
        inputEvent = fe ^. feInputEvent

    (viewModels, shutdown) <- Sodium.sync $ startCore config inputEvent

    putStrLn "Proceeding to frontend's main loop"
    (fe ^. feMainLoop) viewModels
    shutdown
