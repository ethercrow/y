{-# LANGUAGE LambdaCase #-}
module Y.Frontend.Toy where

import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, void)
import qualified FRP.Sodium as Sodium
import Y.Common
import Y.Frontend

startToyFrontend :: IO Frontend
startToyFrontend = do
    (ev, push) <- Sodium.sync Sodium.newEvent

    void . forkIO $ do
        putStrLn "Started input thread"
        forM_ "axbycxdyefghijkzabc" $ \c -> do
            threadDelay 300000
            Sodium.sync $ push (KChar c)
        Sodium.sync $ push KEsc

    let mainLoop vmEvent = do
            exitMVar <- newEmptyMVar

            void . Sodium.sync . Sodium.listen vmEvent $ \case
                OutputViewModel vm -> print vm
                OutputExit -> putMVar exitMVar ()
                _ -> return ()

            takeMVar exitMVar

    return $! Frontend ev mainLoop
