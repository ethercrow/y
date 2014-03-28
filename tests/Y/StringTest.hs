module Y.StringTest where

import Test.HUnit
import Test.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty (TestTree, testGroup)

import Data.Monoid
import qualified Y.String as S

newline :: S.YiString
newline = S.singleton '\n'

tests :: TestTree
tests = testGroup "Rope"
    [ testProperty "conversion" $
        \s -> S.toString (S.fromString s) == s
    , testProperty "reverse" $
        \s -> S.fromString (reverse s) == S.reverse (S.fromString s)
    , testProperty "null" $
        \s -> null s == S.null (S.fromString s)
    , testProperty "take" $
        \s i -> i >= 0 ==> S.fromString (take i s) == S.take i (S.fromString s)
    , testProperty "drop" $
        \s i -> i >= 0 ==> S.fromString (drop i s) == S.drop i (S.fromString s)
    , testProperty "length" $
        \s -> S.Size (fromIntegral (length s)) == S.length (S.fromString s)
    , testProperty "append" $
        \s t -> S.fromString (s ++ t) == S.append (S.fromString s) (S.fromString t)
    , testProperty "concat" $
        \ss -> S.fromString (concat ss) == S.concat (map S.fromString ss)
    , testProperty "countNewLines" $
        \s -> length (filter (== '\n') s) == S.countNewLines (S.fromString s)
    , testProperty "splitAt" $
        \s i -> i >= 0 ==> S.splitAt i (S.fromString s) ==
            (let (x, y) = splitAt i s in (S.fromString x, S.fromString y))
    , testProperty "splitAtLine 0" $
        \s -> let r = S.fromString s in S.splitAtLine 0 r == (S.empty, r)
    , testProperty "splitAtLine 1" $
        \s t -> '\n' `notElem` s ==>
            let r = S.fromString s
                q = S.fromString t
            in S.splitAtLine 1 (r <> newline <> q) == (r <> newline, q)
    , testProperty "splitAtLine i" $
        \s i -> i >= 0 ==>
            let rq = S.fromString s
                (r, q) = S.splitAtLine i rq
            in rq == r <> q
    , testProperty "splitAtLine N" $
        \s ->
            let rq = S.fromString s
                (r, q) = S.splitAtLine (S.countNewLines rq + 1) rq
            in (r, q) == (rq, mempty)
    , testCase "singleton" $
        S.fromString "\n" @=? newline
    , testCase "splitAtLine 1 'a\\nb'" $
        S.splitAtLine 1 (S.fromString "a\nb") @?= (S.fromString "a\n", S.fromString "b")
    , testCase "splitAtLine 1 '\\n'" $
        S.splitAtLine 1 (S.fromString "\n") @?= (S.fromString "\n", S.empty)
    , testCase "splitAtLine 1 '\\n\\n'" $
        S.splitAtLine 1 (S.fromString "\n\n") @?= (newline, newline)
    ]

