{-# LANGUAGE LambdaCase #-}

module Y.Frontend.Vty where

import Control.Concurrent
import Control.Lens
import Control.Monad (forever, void)
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

            unlisten <- Sodium.sync $ Sodium.listen outputEvent (putMVar outputMVar)

            void . forkIO $ do
                putStrLn "Started input thread"
                forever $ do
                    vtyInput <- Vty.next_event vty

                    let minput = convertInput vtyInput

                    case minput of
                        Just input -> Sodium.sync $ pushInput input
                        Nothing -> return ()

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

render :: Vty.Vty -> S.YiString -> IO ()
render vty s = do
    s
        & S.toString
        & lines
        & map (\x -> Vty.string Vty.def_attr (if null x then " " else x))
        & Vty.vert_cat
        & Vty.pic_for_image
        & Vty.update vty
    Vty.refresh vty

convertInput :: Vty.Event -> Maybe InputOccurrence
convertInput (Vty.EvKey Vty.KEsc []) = Just KEsc
convertInput (Vty.EvKey Vty.KEnter []) = Just KEnter
convertInput (Vty.EvKey (Vty.KASCII c) []) = Just (KChar c)
convertInput _ = Nothing
