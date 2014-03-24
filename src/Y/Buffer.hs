{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y.Buffer where

import Control.Lens.TH
import Data.Default

import Y.Common
import Y.String

data Buffer = Buffer
    { _text :: YiString
    , _overlays :: [BufferOverlay]
    , _cursorPosition :: Position 
    }

makeLenses ''Buffer

instance Default Buffer where
    def = Buffer "" [] (Position 0)

data BufferFunctionRO a = BufferFunctionRO (Buffer -> a)
data BufferFunctionRW a = BufferFunctionRW (Buffer -> (a, Buffer))
