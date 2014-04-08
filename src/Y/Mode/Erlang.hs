{-# LANGUAGE OverloadedStrings #-}
module Y.Mode.Erlang where

import Y.Highlighter.Kate
import Y.Mode

erlangMode :: Mode
erlangMode = Mode highlighter Indenter
    where Just highlighter = kateHighlighter "Erlang"
