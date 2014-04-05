{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y.Buffer where

import Control.Lens.TH
import Data.Default
import qualified Data.Vector as V

import Y.String

data Buffer = Buffer
    { _text :: YiString
    , _cursorPosition :: Position 
    }

makeLenses ''Buffer

instance Default Buffer where
    def = Buffer "" 0

data BufferUpdate
    = Composite (V.Vector BufferUpdate)
    | Insert YiString
    | Delete YiString
    | CursorFromTo Position Position
