module Y.Output where

import Control.Lens

import Y.Buffer
import Y.Common
import Y.CoreState
import Y.String

makeOutput :: CoreState -> CoreOutput
makeOutput (CoreState b overlays viewportSize)
    = OutputViewModel (ViewModel textLines cursor (fmap wrapOverlayLines overlays))
    where
        cursor = Just (over both fromIntegral
                            (coordsOfPositionWrappingToWidth pos w txt))
        textLines = wrappedLinesForWidth w txt
        txt = takeScreenful w h (b ^. text)
        pos = b ^. cursorPosition
        wrapOverlayLines = id
        (w, h) = over both fromIntegral viewportSize
