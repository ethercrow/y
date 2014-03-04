{-# LANGUAGE LambdaCase #-}

module Y.Frontend.Vty where

import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar)
import Control.Lens
import Control.Monad (forM_, void)
import Data.Default
import qualified FRP.Sodium as Sodium
import qualified Graphics.Vty as Vty
import Y.Common
import Y.Frontend
import Y.String

startVtyFrontend :: IO Frontend
startVtyFrontend = do
    (ev, push) <- Sodium.sync Sodium.newEvent

    vty <- Vty.mkVty

    -- input thread
    void . forkIO $ do
        putStrLn "Started input thread"
        forM_ "axbycxdyefghijkzabc" $ \c -> do
            threadDelay 300000
            Sodium.sync $ push (KChar c)
        Sodium.sync $ push KEsc

    let mainLoop vmEvent = do
            exitMVar <- newEmptyMVar

            void . Sodium.sync . Sodium.listen vmEvent $ \case
                OutputViewModel (ViewModel s) -> do
                        s & toString
                          & Vty.string Vty.def_attr
                          & Vty.pic_for_image
                          & Vty.update vty
                        Vty.refresh vty
                OutputExit -> do
                    putMVar exitMVar ()
                _ -> return ()

            takeMVar exitMVar

    return $! Frontend ev mainLoop
