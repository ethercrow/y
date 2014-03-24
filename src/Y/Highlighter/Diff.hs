{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Y.Highlighter.Diff where

import Control.Lens
import qualified Data.Vector as V

import Y.Common
import Y.Highlighter
import qualified Y.String as S

diffHighlighter :: Highlighter
diffHighlighter = Highlighter $ \s -> do
    let ls = s
           & S.toString
           & lines
           & V.fromList
           & V.map (\case
                      '+' : _ -> Green
                      '-' : _ -> Red
                      _ -> Default)
           & V.map LineOverlay

    return $! BufferOverlay "Diff" ls 
