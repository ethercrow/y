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
import Y.Mode.Erlang
import qualified Y.String as S

import Y.Frontend.Toy
import Y.Frontend.Vty

import Y.Keymap.Toy

main :: IO ()
main = do
    maybeFilename <- listToMaybe <$> getArgs
    fe <- startVtyFrontend

    let config = Config toyKeymap [erlangMode]
        inputEvent = fe ^. feInputEvent

    (viewModels, shutdown) <- case maybeFilename of
        Just filename -> do
            content <- TIO.readFile filename
            let showTextAction = SyncA (StateModA (buffer . text .~ S.fromLazyText content))
            Sodium.sync $ startCore config inputEvent [showTextAction]
        Nothing -> Sodium.sync $ startCore config inputEvent []

    putStrLn "Proceeding to frontend's main loop"
    (fe ^. feMainLoop) viewModels
    shutdown
