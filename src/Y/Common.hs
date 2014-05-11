module Y.Common where

import Control.DeepSeq
import Data.Monoid
import qualified Data.Vector as V

import Y.String

data InputOccurrence
    = KChar Char
    | KEsc
    | KEnter
    | KUp | KDown | KRight | KLeft
    | ViewportResize { width :: !Int, height :: !Int }
    deriving Show

data ViewModel = ViewModel
    { _vmLines :: V.Vector YiString
    , _vmCursor :: Maybe (Int, Int)
    , _vmOverlays :: [BufferOverlay]
    }
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
    rnf (LineOverlay _) = ()

data Color
    = Default
    | Red
    | Green
    deriving (Show, Eq)

data CoreOutput
    = OutputViewModel ViewModel
    | OutputNoop
    | OutputExit

