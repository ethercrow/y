module Main where

import Control.Concurrent
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

    inputEvents <- inputEvent toyFrontend

    let config = Config toyKeymap

    viewModels <- Sodium.sync (startCore config inputEvents exitMVar)
    void $ Sodium.sync $ Sodium.listen viewModels (render toyFrontend)

    putStrLn "Waiting for exit"
    void $ takeMVar exitMVar
