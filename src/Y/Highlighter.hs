module Y.Highlighter where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.DeepSeq
import Control.Lens hiding (Action)
import Control.Monad
import qualified FRP.Sodium as Sodium

import Y.Buffer
import Y.Common
import Y.CoreState
import Y.Keymap
import Y.SodiumUtils
import Y.String

newtype Highlighter = Highlighter (YiString -> IO BufferOverlay)

asyncHighlightAction :: Highlighter -> Action
asyncHighlightAction (Highlighter h)
    = AsyncA $ \state -> do
        let txt = state ^. csBuffer . text
        o <- h txt
        return $! SyncA $ StateModA $ \state' ->
            if state' ^. csBuffer . text == txt
            then state' & csOverlays .~ [o]
            else state'

highlightEventForBuffer :: Sodium.Behavior Buffer -> Highlighter -> Sodium.Reactive (Sodium.Event Action)
highlightEventForBuffer bufferBehavior (Highlighter h) = do
    (actionEvent, pushAction) <- Sodium.newEvent
    (availableBehavior, pushAvailable) <- Sodium.newBehavior True
    bufs <- uniqueUpdates bufferBehavior

    let highlightWhenAvailable :: Buffer -> IO ()
        highlightWhenAvailable b = void . forkIO $ do
            isHighlighterAvailable <- Sodium.sync $ do
                isHighlighterAvailable' <- Sodium.sample availableBehavior
                when isHighlighterAvailable' $
                    pushAvailable False
                return isHighlighterAvailable'
            when isHighlighterAvailable $ do
                overlay <- force <$> h (b ^. text)
                Sodium.sync $ do
                    pushAction (SyncA (StateModA (\state ->
                                if state ^. csBuffer == b
                                then state & csOverlays .~ [overlay]
                                else state)))
                    pushAvailable True

    _ <- Sodium.listen bufs highlightWhenAvailable
    return actionEvent
