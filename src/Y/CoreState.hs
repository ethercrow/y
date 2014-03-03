{-# LANGUAGE TemplateHaskell #-}

module Y.CoreState where

import Control.Lens.TH

import Y.Buffer

data CoreState = CoreState
    { _buffer :: Buffer
    }

makeLenses ''CoreState
