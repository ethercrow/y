{-# LANGUAGE TemplateHaskell #-}

module Y.CoreState where

import Control.Concurrent
import Control.Lens.TH

import Y.Buffer

data CoreState = CoreState
    { _someBool :: Bool
    , _exitMVar :: MVar ()
    , _buffer :: Buffer
    }

makeLenses ''CoreState
