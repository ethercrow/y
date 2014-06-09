{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Y.StringTest where

import Test.HUnit
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Control.Applicative
import Data.Foldable (foldMap)
import Data.Monoid
import qualified Data.Text.Lazy as TL

import qualified Y.String as S

newline :: S.YiString
newline = S.singleton '\n'

tests :: TestTree
tests = $(testGroupGenerator)

prop_string_conversion s
    = S.toString (S.fromString s) == s

prop_text_conversion s
    = S.toLazyText (S.fromLazyText (TL.pack s)) == TL.pack s

prop_reverse s
    = S.fromString (reverse s) == S.reverse (S.fromString s)

prop_null s
    = null s == S.null (S.fromString s)

prop_take s i
    = i >= 0 ==>
      S.fromString (take i s) == S.take (fromIntegral i) (S.fromString s)

prop_drop s i
    = i >= 0 ==>
      S.fromString (drop i s) == S.drop (S.Size (fromIntegral i)) (S.fromString s)

prop_length s
    = length s == fromIntegral (S.fromSize (S.length (S.fromString s)))

prop_append s t
    = S.fromString (s ++ t) == S.append (S.fromString s) (S.fromString t)

prop_append_nl s t
    = S.fromString (fromSWLON s ++ fromSWLON t)
        == S.append (S.fromString (fromSWLON s)) (S.fromString (fromSWLON t))

prop_concat ss
    = S.fromString (concat ss) == S.concat (map S.fromString ss)

prop_countNewLines s
    = fromIntegral (length (filter (== '\n') s)) == S.countNewLines (S.fromString s)

prop_splitAt s i
    = i >= 0 ==>
      let (x, y) = splitAt i (fromSWLON s)
      in S.splitAt (fromIntegral i) (S.fromString (fromSWLON s))
        == (S.fromString x, S.fromString y)

prop_splitAtLine_0 s
    = let r = S.fromString (fromSWLON s) in S.splitAtLine 0 r == (mempty, r)

prop_splitAtLine_1 s t
    = '\n' `notElem` s ==>
      let r = S.fromString s
          q = S.fromString t
      in S.splitAtLine 1 (r <> newline <> q) == (r <> newline, q)

prop_splitAtLine_i (s :: StringWithLotsOfNewlines) i
    = i >= 0 ==>
      let rq = S.fromString (fromSWLON s)
          i' = i `rem` (1 + S.countNewLines rq)
          (r, q) = S.splitAtLine i' rq
      in rq == r <> q

prop_splitAtLine_N s
    = let rq = S.fromString s
          (r, q) = S.splitAtLine (S.countNewLines rq + 1) rq
      in (r, q) == (rq, mempty)

prop_insertAt i s t
    = i <= length t ==>
      let r = S.fromString s
          q = S.fromString t
          rq = S.insertAt r (fromIntegral i) q
      in rq == S.fromString (take i t <> s <> drop i t)

prop_delete_zero i s
    = let r = S.fromString s
          i' = i `rem` (S.fromSize (S.length r) + 1)
      in r == S.deleteAt (fromIntegral i') (S.Size 0) r

prop_deleteAt i l s
    = i >= 0 && l >= 0 && not (null s) ==>
      let i' = i `rem` length s
          l' = l `rem` (length s - i')
          r = S.fromString s
          r' = S.deleteAt (fromIntegral i') (S.Size (fromIntegral l')) r
      in r' == S.fromString (take i' s <> drop (i' + l') s)

prop_insert_delete i s t
    = i >= 0 && not (null t) ==>
      let r = S.fromString s
          q = S.fromString t
          i' = i `rem` fromIntegral (length t)
          rq = S.insertAt r i' q
      in q == S.deleteAt i' (S.length r) rq

prop_concat_singletons s
    = foldMap S.singleton s == S.fromString s

prop_cons c s
    = S.toLazyText (S.cons c (S.fromString s)) == TL.cons c (TL.pack s)

prop_snoc s c
    = S.toLazyText (S.snoc (S.fromString s) c) == TL.snoc (TL.pack s) c

case_singleton = S.fromString "\n" @=? newline

case_splitAt_1_aaa
    = S.splitAt 1 "aaa" @?= ("a", "aa")

case_splitAtLine_1_a_nl_b
    = S.splitAtLine 1 "a\nb" @?= ("a\n", "b")

case_splitAtLine_2
    = S.splitAtLine 2 "a\nb\nc\n" @?= ("a\nb\n", "c\n")

case_splitAtLine_2_nl
    = S.splitAtLine 2 "\n\n\n" @?= ("\n\n", "\n")

case_splitAtLine_1_nl
    = S.splitAtLine 1 "\n" @?= ("\n", S.empty)

case_splitAtLine_1_nl_nl
    = S.splitAtLine 1 "\n\n" @?= (newline, newline)

case_takeScreenful_1_1
    = S.takeScreenful 1 1 "ab\ncd\nef" @?= "a"

case_takeScreenful_1_2
    = S.takeScreenful 1 2 "ab\ncd\nef" @?= "ab"

case_takeScreenful_1_3
    = S.takeScreenful 1 3 "ab\ncd\nef" @?= "ab\nc"

case_takeScreenful_2_1
    = S.takeScreenful 2 1 "ab\ncd\nef" @?= "ab"

case_takeScreenful_2_2
    = S.takeScreenful 2 2 "ab\ncd\nef" @?= "ab\ncd"

case_takeScreenful_3_1
    = S.takeScreenful 3 1 "ab\ncd\nef" @?= "ab"

case_takeScreenful_3_3
    = S.takeScreenful 3 3 "ab\ncd\nef" @?= "ab\ncd\nef"

case_takeScreenful_4_4
    = S.takeScreenful 4 4 "abcd1234567\nfoob\nquux" @?= "abcd1234567\nfoob"

case_takeScreenful_4_4'
    = S.takeScreenful 4 4 "abcd12345678\nfoob\nquux" @?= "abcd12345678\nfoob"

case_coordsOfPosition_1
    = S.coordsOfPositionWrappingToWidth 0 3 "ab\ncd\nef" @?= (0, 0)

case_coordsOfPosition_2
    = S.coordsOfPositionWrappingToWidth 1 3 "ab\ncd\nef" @?= (0, 1)

case_coordsOfPosition_3
    = S.coordsOfPositionWrappingToWidth 2 3 "ab\ncd\nef" @?= (0, 2)

case_coordsOfPosition_4
    = S.coordsOfPositionWrappingToWidth 3 3 "ab\ncd\nef" @?= (1, 0)

case_coordsOfPosition_5
    = S.coordsOfPositionWrappingToWidth 3 3 "abcdef" @?= (1, 0)

newtype StringWithLotsOfNewlines = StringWithLotsOfNewlines { fromSWLON :: String }
    deriving Show

instance Arbitrary StringWithLotsOfNewlines where
    arbitrary = StringWithLotsOfNewlines <$> listOf (elements "a\n")

