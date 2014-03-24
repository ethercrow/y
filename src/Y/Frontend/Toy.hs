module Y.Frontend.Toy where

import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, void)
import qualified FRP.Sodium as Sodium
import Y.Common
import Y.Frontend

startToyFrontend :: IO Frontend
startToyFrontend = do
    (inputEvent, pushInput) <- Sodium.sync Sodium.newEvent

    let mainLoop vmEvent = do
            exitMVar <- newEmptyMVar

            unlistenOutput <- Sodium.sync . Sodium.listen vmEvent $ \e -> case e of
                OutputViewModel vm -> render vm
                OutputExit -> putMVar exitMVar ()
                _ -> return ()

            void . forkIO $ do
                putStrLn "Started input thread"
                forM_ "axbycxdyefghijkzabc" $ \c -> do
                    Sodium.sync $ pushInput (KChar c)
                    threadDelay 300000
                Sodium.sync $ pushInput KEsc

            takeMVar exitMVar
            unlistenOutput

    return $! Frontend inputEvent mainLoop

render :: ViewModel -> IO ()
render = print
