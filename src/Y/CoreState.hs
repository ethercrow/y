{-# LANGUAGE TemplateHaskell #-}

module Y.CoreState where

import Control.Lens.TH

import Y.Common
import Y.Buffer

data CoreState = CoreState
    { _buffer :: Buffer
    , _overlays :: [BufferOverlay]
    }

makeLenses ''CoreState
