module Y.Mode.Fundamental where

import qualified FRP.Sodium as Sodium

import Y.Mode

fundamentalMode :: Mode
fundamentalMode _ = return $! ModeOutput Sodium.never id Indenter