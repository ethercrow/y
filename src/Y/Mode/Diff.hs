{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Y.Mode.Diff where

import Control.Applicative
import Control.Lens
import qualified FRP.Sodium as Sodium
import qualified Data.Vector as V

import Y.Common
import Y.Highlighter
import Y.Keymap
import Y.Mode
import qualified Y.String as S

diffMode :: Mode
diffMode bufBehavior = do
    highlightEvent <- highlightEventForBuffer bufBehavior highlighter
    return $! ModeOutput highlightEvent id Indenter

highlighter :: Highlighter
highlighter = Highlighter $ \s -> do
    let ls = s
             & S.toString
             & lines
             & V.fromList
             & V.map (\case
                        '+' : _ -> Green
                        '-' : _ -> Red
                        _ -> Default)
             & V.map LineOverlay
    return $! BufferOverlay "diff" ls