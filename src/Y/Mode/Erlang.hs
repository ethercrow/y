{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Y.Mode.Erlang where

import Y.Highlighter
import Y.Highlighter.Kate
import Y.Mode

erlangMode :: Mode
erlangMode bufBehavior = do
    let Just highlighter = kateHighlighter "Erlang"
    overlayEvent <- highlightEventForBuffer bufBehavior highlighter
    return $! ModeOutput overlayEvent id Indenter
