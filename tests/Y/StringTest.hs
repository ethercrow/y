{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Y.StringTest where

import Test.HUnit
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH

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
      S.fromString (take i s) == S.take i (S.fromString s)

prop_drop s i
    = i >= 0 ==>
      S.fromString (drop i s) == S.drop i (S.fromString s)

prop_length s
    = length s == S.length (S.fromString s)

prop_append s t
    = S.fromString (s ++ t) == S.append (S.fromString s) (S.fromString t)

prop_concat ss
    = S.fromString (concat ss) == S.concat (map S.fromString ss)

prop_countNewLines s
    = length (filter (== '\n') s) == S.countNewLines (S.fromString s)

prop_splitAt s i
    = i >= 0 ==>
      let (x, y) = splitAt i s
      in S.splitAt i (S.fromString s) == (S.fromString x, S.fromString y)

prop_splitAtLine_0 s
    = let r = S.fromString s in S.splitAtLine 0 r == (mempty, r)

prop_splitAtLine_1 s t
    = '\n' `notElem` s ==>
      let r = S.fromString s
          q = S.fromString t
      in S.splitAtLine 1 (r <> newline <> q) == (r <> newline, q)

prop_splitAtLine_i s i
    = i >= 0 ==>
      let rq = S.fromString s
          (r, q) = S.splitAtLine i rq
      in rq == r <> q

prop_splitAtLine_N s
    = let rq = S.fromString s
          (r, q) = S.splitAtLine (S.countNewLines rq + 1) rq
      in (r, q) == (rq, mempty)

prop_insertAt i s t
    = i <= length t ==>
      let r = S.fromString s
          q = S.fromString t
          rq = S.insertAt r i q
      in rq == S.fromString (take i t <> s <> drop i t)

prop_delete_zero i s
    = let r = S.fromString s
      in r == S.deleteAt i 0 r

prop_deleteAt i l s
    = i >= 0 && l >= 0 ==>
      let i' = i `rem` length s
          l' = l `rem` (length s - i')
          r = S.fromString s
          r' = S.deleteAt i l r
      in r' == S.fromString (take i s <> drop (i + l) s)

prop_insert_delete i s t
    = i <= length t ==>
      let r = S.fromString s
          q = S.fromString t
          rq = S.insertAt r i q
      in q == S.deleteAt i (S.length r) rq

prop_concat_singletons s
    = foldMap S.singleton s == S.fromString s

prop_cons c s
    = S.toLazyText (S.cons c (S.fromString s)) == TL.cons c (TL.pack s)

prop_snoc s c
    = S.toLazyText (S.snoc (S.fromString s) c) == TL.snoc (TL.pack s) c

case_singleton = S.fromString "\n" @=? newline

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

