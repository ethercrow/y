module Y.Common where

import Control.DeepSeq
import Data.Monoid
import qualified Data.Vector as V

import Y.String

data InputOccurrence = KChar Char | KEsc | KEnter
    deriving Show

data ViewModel = ViewModel YiString (Maybe (Int, Int)) [BufferOverlay]
    deriving (Show, Eq)

data BufferOverlay = BufferOverlay
    { _boName :: !YiString
    , _boLines :: !(V.Vector LineOverlay)
    } deriving (Show, Eq)

instance NFData BufferOverlay where
    rnf (BufferOverlay n ls) = rnf n `seq` rnf ls

emptyOverlay :: YiString -> BufferOverlay
emptyOverlay name = BufferOverlay name mempty

data LineOverlay = LineOverlay !Color
    deriving (Show, Eq)

instance NFData LineOverlay where
    rnf (LineOverlay c) = ()

data Color
    = Default
    | Red
    | Green
    deriving (Show, Eq)

data CoreOutput
    = OutputViewModel ViewModel
    | OutputNoop
    | OutputExit

