{-# LANGUAGE TemplateHaskell #-}

module Y.CoreState where

import Control.Lens.TH

import Y.Buffer
import Y.Common

data CoreState = CoreState
    { _csBuffer :: Buffer
    , _csOverlays :: [BufferOverlay]
    , _csViewportSize :: (Int, Int)
    } deriving Eq

makeLenses ''CoreState
