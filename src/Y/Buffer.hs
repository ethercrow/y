{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y.Buffer where

import Control.Lens
import Control.Lens.TH
import Data.Default
import Data.Group
import Data.Monoid
import qualified Data.Vector as V

import qualified Y.String as S

data Buffer = Buffer
    { _text :: S.YiString
    , _cursorPosition :: S.Position
    } deriving (Eq, Show)

makeLenses ''Buffer

instance Default Buffer where
    def = Buffer "" 0

data BufferUpdate
    = Composite (V.Vector BufferUpdate)
    | Insert S.YiString
    | Delete S.YiString
    | CursorFromTo S.Position S.Position
    | Nop
    deriving (Show, Eq)

cursorUp :: Buffer -> BufferUpdate
cursorUp (Buffer string cursor)
    = if y > 0
      then CursorFromTo cursor (S.positionForCoords (pred y, 0) string)
      else Nop
    where
        (y, x) = S.coordsOfPosition cursor string

instance Monoid BufferUpdate where
    mempty = Nop
    mappend x Nop = x
    mappend Nop x = x
    mappend (Insert s) (Insert t) = Insert (mappend s t)
    mappend (Composite xs) (Composite ys) = Composite (mappend xs ys)
    mappend x y = Composite (V.fromList [x, y])

instance Group BufferUpdate where
    invert Nop = Nop
    invert (Insert s) = Delete s
    invert (Delete s) = Insert s
    invert (CursorFromTo x y) = CursorFromTo y x
    invert (Composite xs) = Composite (V.reverse (fmap invert xs))

cursorDown :: Buffer -> BufferUpdate
cursorDown (Buffer string cursor)
    = if y < lineCount
      then CursorFromTo cursor (S.positionForCoords (succ y, 0) string)
      else Nop
    where
        (y, x) = S.coordsOfPosition cursor string
        lineCount = S.countNewLines string

cursorLeft :: Buffer -> BufferUpdate
cursorLeft b | atSof b = Nop
cursorLeft (Buffer _ cursor) = CursorFromTo cursor (pred cursor)

cursorRight :: Buffer -> BufferUpdate
cursorRight b | atEof b = Nop
cursorRight (Buffer _ cursor) = CursorFromTo cursor (succ cursor)

atSof :: Buffer -> Bool
atSof (Buffer _ 0) = True
atSof _ = False

atEof :: Buffer -> Bool
atEof (Buffer string cursor) = cursor == S.fromSize (S.length string)
