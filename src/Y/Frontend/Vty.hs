{-# LANGUAGE LambdaCase #-}

module Y.Frontend.Vty where

import Control.Concurrent
import Control.Lens
import Control.Monad (forM_, void, forever)
import Data.Default
import qualified FRP.Sodium as Sodium
import qualified Graphics.Vty as Vty

import Y.Common
import Y.Frontend
import qualified Y.String as S

startVtyFrontend :: IO Frontend
startVtyFrontend = do
    (inputEvent, pushInput) <- Sodium.sync Sodium.newEvent
    vty <- Vty.mkVty

    let mainLoop outputEvent = do
            outputMVar <- newEmptyMVar

            unlisten <- Sodium.sync $ Sodium.listen outputEvent $ \o -> do
                putMVar outputMVar o

            void . forkIO $ do
                putStrLn "Started input thread"

                forM_ "axbycxdyefghijkzabc" $ \c -> do
                    Sodium.sync $ pushInput (KChar c)
                    yield
                    threadDelay 300000
                Sodium.sync $ pushInput KEsc

            let loop = do
                    output <- takeMVar outputMVar
                    case output of
                        OutputViewModel (ViewModel s) -> render vty s >> loop
                        OutputExit -> return ()
                        _ -> yield >> threadDelay 1000000 >> loop
            loop
            unlisten
            Vty.shutdown vty

    return $! Frontend inputEvent mainLoop

render vty s = do
    let outputString = unwords [show (S.length s), S.toString s]
    outputString
        & Vty.string Vty.def_attr
        & Vty.pic_for_image
        & Vty.update vty
    Vty.refresh vty
