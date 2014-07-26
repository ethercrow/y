{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Y.BufferTest where

import Test.HUnit
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import qualified Y.Buffer as B

tests :: TestTree
tests = $(testGroupGenerator)

someBuffer :: B.Buffer
someBuffer = B.Buffer "abc\ndef\nghi" 6

case_cursorLeft
    = B.CursorFromTo 6 5 @=? B.cursorLeft someBuffer
case_cursorLeft_at_sof
    = B.Nop @=? B.cursorLeft (B.Buffer "abc\ndef\nghi" 0)

case_cursorRight
    = B.CursorFromTo 6 7 @=? B.cursorRight someBuffer
case_cursorRight_at_eof
    = B.Nop @=? B.cursorRight (B.Buffer "abc\ndef\nghi" 11)

case_cursorUp
    = B.CursorFromTo 6 0 @=? B.cursorUp someBuffer
case_cursorUp_at_top
    = B.Nop @=? B.cursorUp (B.Buffer "abc\ndef\nghi" 2)

case_cursorDown
    = B.CursorFromTo 6 8 @=? B.cursorDown someBuffer
case_cursorDown_at_bottom
    = B.Nop @=? B.cursorDown (B.Buffer "abc\ndef\nghi" 11)
