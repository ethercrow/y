module Main where

import Control.Concurrent
import Control.Lens
import Control.Monad
import qualified FRP.Sodium as Sodium

import Y.Core
import Y.Frontend
import Y.Config

import Y.Frontend.Toy
import Y.Keymap.Toy

main :: IO ()
main = do
    exitMVar <- newEmptyMVar
    putStrLn "Started"

    inputEvents <- toyFrontend ^. feInputEvent

    let config = Config toyKeymap

    viewModels <- Sodium.sync (startCore config inputEvents exitMVar)
    void $ Sodium.sync $ Sodium.listen viewModels (toyFrontend ^. feRender)

    putStrLn "Waiting for exit"
    void $ takeMVar exitMVar
