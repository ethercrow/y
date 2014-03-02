{-# LANGUAGE TemplateHaskell #-}

-- | Everything modifiable at runtime.
module Y.Config where

import Control.Lens.TH

import Y.Keymap

data Config = Config
    { _cfgKeymap :: Keymap
    }

makeLenses ''Config
