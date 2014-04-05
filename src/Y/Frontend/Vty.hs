{-# LANGUAGE LambdaCase #-}

module Y.Frontend.Vty where

import Control.Concurrent
import Control.Lens
import Control.Monad (forever, void)
import qualified FRP.Sodium as Sodium
import qualified Graphics.Vty as Vty
import qualified Data.Vector as V

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
                        OutputViewModel vm -> render vty vm >> loop
                        OutputExit -> return ()
                        _ -> yield >> threadDelay 1000000 >> loop
            loop
            unlisten
            Vty.shutdown vty

    return $! Frontend inputEvent mainLoop

render :: Vty.Vty -> ViewModel -> IO ()
render vty (ViewModel s mcursor overlays) = do
    let uncoloredLines = lines (S.toString s)
                       & map (\x -> if null x then " " else x)

        coloredLines = foldr applyOverlay
                             (zip uncoloredLines (repeat Default))
                             overlays
        applyOverlay (BufferOverlay _ los)
            = zipWith (\(LineOverlay ocolor) (l, color)
                            -> case ocolor of
                                Default -> (l, color)
                                _ -> (l, ocolor))
                      (V.toList los)

        image = coloredLines
              & map (\(x, color) -> Vty.string (colorToAttr color) x)
              & Vty.vert_cat
        cursor = case mcursor of
            Nothing -> Vty.NoCursor
            Just (x, y) -> Vty.Cursor (fromIntegral y) (fromIntegral x)
        background = Vty.Background ' ' Vty.def_attr
        picture = Vty.Picture cursor image background

    Vty.update vty picture
    Vty.refresh vty

convertInput :: Vty.Event -> Maybe InputOccurrence
convertInput (Vty.EvKey Vty.KEsc []) = Just KEsc
convertInput (Vty.EvKey Vty.KEnter []) = Just KEnter
convertInput (Vty.EvKey (Vty.KASCII c) []) = Just (KChar c)
convertInput _ = Nothing

colorToAttr :: Color -> Vty.Attr
colorToAttr Default = Vty.def_attr
colorToAttr Red = Vty.with_fore_color Vty.def_attr Vty.red
colorToAttr Green = Vty.with_fore_color Vty.def_attr Vty.green
