{-# LANGUAGE TemplateHaskell #-}
module Y.Mode where

import Control.Lens.TH
import Y.Highlighter

data Indenter = Indenter

data Mode = Mode
    { _modeHighlighter :: Highlighter
    , _modeIndenter :: Indenter
    }

makeLenses ''Mode
