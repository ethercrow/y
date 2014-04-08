{-# LANGUAGE TemplateHaskell #-}
module Y.Mode where

import Control.Lens.TH
import FRP.Sodium as Sodium

import Y.Buffer
import Y.Keymap

data Indenter = Indenter

type Mode = Sodium.Behavior Buffer -> Sodium.Reactive ModeOutput

data ModeOutput = ModeOutput
    { _moActionEvent :: Sodium.Event Action
    , _moKeymapMod :: Keymap -> Keymap
    , _moIndenter :: Indenter
    }

makeLenses ''ModeOutput
