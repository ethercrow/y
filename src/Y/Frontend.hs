{-# LANGUAGE TemplateHaskell #-}

module Y.Frontend where

import Control.Lens.TH
import qualified FRP.Sodium as Sodium

import Y.Common

data Frontend = Frontend
    { _feInputEvent :: Sodium.Event InputOccurrence
    , _feMainLoop :: Sodium.Event CoreOutput -> IO ()
    }

makeLenses ''Frontend
