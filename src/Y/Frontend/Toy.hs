module Y.Frontend.Toy where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_)
import qualified FRP.Sodium as Sodium
import Y.Frontend

toyFrontend :: Frontend
toyFrontend = Frontend makeEvent print
    where makeEvent = do
            (ev, push) <- Sodium.sync Sodium.newEvent
            forkIO $ do
                putStrLn "Started frontend thread"
                forM_ "abcdefghijkzabc" $ \c -> do
                    threadDelay 300000
                    Sodium.sync $ push (KChar c)
                Sodium.sync $ push KEsc
            return ev
