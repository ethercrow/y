module Y.Common where

import qualified Data.Vector as V

import Y.String

data InputOccurrence = KChar Char | KEsc | KEnter
    deriving Show

data ViewModel = ViewModel YiString (Maybe (Int, Int)) [BufferOverlay]
    deriving Show

data BufferOverlay = BufferOverlay
    { _boName :: YiString
    , _boLines :: V.Vector LineOverlay
    }
    deriving Show

data LineOverlay = LineOverlay Color
    deriving Show

data Color
    = Default
    | Red
    | Green
    deriving Show

data CoreOutput
    = OutputViewModel ViewModel
    | OutputNoop
    | OutputExit

