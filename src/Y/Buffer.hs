{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y.Buffer where

import Control.Lens
import Control.Lens.TH
import Data.Default
import qualified Data.Vector as V

import qualified Y.String as S

data Buffer = Buffer
    { _text :: S.YiString
    , _cursorPosition :: S.Position
    } deriving Eq

makeLenses ''Buffer

instance Default Buffer where
    def = Buffer "" 0

data BufferUpdate
    = Composite (V.Vector BufferUpdate)
    | Insert S.YiString
    | Delete S.YiString
    | CursorFromTo S.Position S.Position

cursorUp :: Buffer -> Buffer
cursorUp (Buffer string cursor) = Buffer string cursor'
    where
        (y, x) = S.coordsOfPosition cursor string
        cursor' = S.positionForCoords (pred y, x) string

cursorDown :: Buffer -> Buffer
cursorDown (Buffer string cursor) = Buffer string cursor'
    where
        (y, x) = S.coordsOfPosition cursor string
        cursor' = S.positionForCoords (succ y, x) string

cursorLeft :: Buffer -> Buffer
cursorLeft b | atSof b = b
cursorLeft b = b & cursorPosition %~ pred

cursorRight :: Buffer -> Buffer
cursorRight b | atEof b = b
cursorRIght b = b & cursorPosition %~ succ

atSof :: Buffer -> Bool
atSof (Buffer _ 0) = True
atSof _ = False

atEof :: Buffer -> Bool
atEof (Buffer string cursor) = cursor == S.fromSize (S.length string)
