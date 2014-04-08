{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Y.Mode.Erlang where

import Control.Applicative
import Control.Lens
import Data.Default
import qualified FRP.Sodium as Sodium
import qualified FRP.Sodium.IO as Sodium

import Y.Buffer
import Y.Common
import Y.Highlighter
import Y.Highlighter.Kate
import Y.Mode

erlangMode :: Mode
erlangMode bufBehavior = do
    let Just highlighter = kateHighlighter "Erlang"
    overlayEvent <- highlightEventForBuffer bufBehavior highlighter
    return $! ModeOutput overlayEvent id Indenter
