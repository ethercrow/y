{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Y.Core
    ( startCore
    , CoreOutput(..)
    ) where

import Control.Applicative
import Control.Lens hiding (Action)
import Data.Default
import Data.Function (on)
import qualified FRP.Sodium as Sodium
import qualified FRP.Sodium.IO as SodiumIO

import Y.Buffer
import Y.Common
import Y.Config
import Y.CoreState
import Y.Highlighter
import Y.Keymap
import Y.String

startCore :: Config
    -> Sodium.Event InputOccurrence
    -> [Action]
    -> Sodium.Reactive (Sodium.Event CoreOutput, IO ())
startCore config inputEvent startupActions = do
    rec
        configBehaviour <- Sodium.accum config configModEvent
        stateBehaviour <- Sodium.accum (CoreState def def) stateModEvent
        (startupActionEvent, pushStartupAction) <- Sodium.newEvent

        let actionEvent = foldr1 Sodium.merge
                            [ startupActionEvent
                            , (Sodium.snapshot (\i conf -> applyKeymap i (conf ^. cfgKeymap))
                                            inputEvent
                                            configBehaviour)
                            , asyncActionEvent
                            , highlighterActionEvent
                            ]

            syncActionEvent = actionEvent
                            & Sodium.filterE isSync
                            & fmap (\(SyncA x) -> x)

            asyncActionEvent = actionEvent
                             & Sodium.filterE (not . isSync)
                             & fmap (\(AsyncA x) -> do
                                        currentState <- Sodium.sync $ Sodium.sample stateBehaviour
                                        x currentState)
                             & SodiumIO.executeAsyncIO

            outputEvent = Sodium.merge
                (fmap (\s -> let txt = s ^. buffer . text
                                 pos = s ^. buffer . cursorPosition
                             in OutputViewModel
                                   (ViewModel txt
                                              (Just (coordsOfPosition pos 80 txt))
                                              (s ^. overlays)))
                    (Sodium.value stateBehaviour))
                (fmap (const OutputExit)
                    (Sodium.filterE isExit syncActionEvent))

            configModEvent = Sodium.filterJust
                           $ fmap (\action -> case action of
                                        KeymapModA f -> Just (over cfgKeymap f)
                                        _ -> Nothing)
                                  syncActionEvent

            stateModEvent = Sodium.filterJust
                          $ fmap (\action -> case action of
                                        StateModA f -> Just f
                                        _ -> Nothing)
                                 syncActionEvent

        textUpdates <- Sodium.updates (view (buffer . text) <$> stateBehaviour)
                     & Sodium.collectE (\curr prev -> ( if curr /= prev then Just curr else Nothing
                                                      , curr))
                                       ""
                     & fmap Sodium.filterJust

        let highlighterActionEvent
                = textUpdates
                & fmap (const
                     (AsyncA $ \state -> do
                         let (Highlighter highlight) = config ^. cfgHighlighter
                         overlay <- highlight (state ^. (buffer . text))
                         return $ SyncA (StateModA (\state' ->
                                     if ((==) `on` (view (buffer . text))) state state'
                                     then state' & overlays .~ [overlay]
                                     else state'))))

    mapM_ pushStartupAction startupActions

    return (outputEvent, return ())

isExit :: SyncAction -> Bool
isExit ExitA = True
isExit _ = False
