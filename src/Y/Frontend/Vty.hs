{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Y.Frontend.Vty where

import Control.Concurrent
import Control.Lens
import Control.Monad (forever, void)
import Data.Monoid
import qualified Data.Vector as V
import qualified FRP.Sodium as Sodium
import qualified Graphics.Vty as Vty

import Y.Common
import Y.Frontend
import qualified Y.String as S

startVtyFrontend :: IO Frontend
startVtyFrontend = do
    (inputEvent, pushInput) <- Sodium.sync Sodium.newEvent
    vty <- Vty.mkVty (Vty.Config 0)

    let mainLoop outputEvent = do
            outputMVar <- newEmptyMVar

            unlisten <- Sodium.sync $ Sodium.listen outputEvent (putMVar outputMVar)

            void . forkIO $ do
                putStrLn "Started input thread"
                forever $ do
                    vtyInput <- Vty.nextEvent vty

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
render vty (ViewModel ls mcursor overlays) = do
    let uncoloredLines = V.map (`S.snoc` ' ') ls

        coloredLines = foldr applyOverlay
                             (V.map (, Default) uncoloredLines)
                             overlays
        applyOverlay :: BufferOverlay -> V.Vector (S.YiString, Color) -> V.Vector (S.YiString, Color)
        applyOverlay (BufferOverlay _ los) clines
            = V.zipWith (\(LineOverlay ocolor) (l, color)
                            -> case ocolor of
                                Default -> (l, color)
                                _ -> (l, ocolor))
                      (los <> V.replicate (V.length clines - V.length los)
                                          (LineOverlay Default))
                      clines

        image = coloredLines
              & V.map (\(x, color) -> vtyYString (colorToAttr color) x)
              & V.toList
              & Vty.vertCat
        cursor = case mcursor of
            Nothing -> Vty.NoCursor
            Just (x, y) -> Vty.Cursor (fromIntegral y) (fromIntegral x)
        background = Vty.Background ' ' Vty.defAttr
        picture = Vty.Picture cursor [image] background

    Vty.update vty picture
    Vty.refresh vty

convertInput :: Vty.Event -> Maybe InputOccurrence
convertInput (Vty.EvKey Vty.KEsc []) = Just KEsc
convertInput (Vty.EvKey Vty.KEnter []) = Just KEnter
convertInput (Vty.EvKey (Vty.KChar c) []) = Just (KChar c)
convertInput _ = Nothing

colorToAttr :: Color -> Vty.Attr
colorToAttr Default = Vty.defAttr
colorToAttr Red = Vty.withForeColor Vty.defAttr Vty.red
colorToAttr Green = Vty.withForeColor Vty.defAttr Vty.green

vtyYString :: Vty.Attr -> S.YiString -> Vty.Image
vtyYString attr = Vty.text attr . S.toLazyText
