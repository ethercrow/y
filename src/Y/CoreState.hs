{-# LANGUAGE TemplateHaskell #-}

module Y.CoreState where

import Control.Lens.TH

import Y.Buffer
import Y.Common

data CoreState = CoreState
    { _buffer :: Buffer
    , _overlays :: [BufferOverlay]
    } deriving Eq

makeLenses ''CoreState
