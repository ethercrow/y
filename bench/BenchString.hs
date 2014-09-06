{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Criterion.Main as C
import GHC.Int
import Control.DeepSeq
import Data.List (foldl')
import qualified Y.String as S

benchCons :: String -> String -> C.Benchmark
benchCons text name
    = C.bench name $ C.nf go ""
    where go start = foldr S.cons start text

benchSnoc :: String -> String -> C.Benchmark
benchSnoc text name
    = C.bench name $ C.nf go ""
    where go start = foldl' S.snoc start text

benchLength :: S.YiString -> String -> C.Benchmark
benchLength text name
    = C.bench name
    $ C.nf S.length text

benchLines :: S.YiString -> String -> C.Benchmark
benchLines text name
    = C.bench name
    $ C.nf (S.splitOnNewLines :: S.YiString -> [S.YiString]) text

benchDrop :: S.YiString -> String -> C.Benchmark
benchDrop text name
    = C.bench name
    $ C.nf (\x -> foldr S.drop x (replicate 1000 (1 :: Int64))) text

benchTake :: S.YiString -> String -> C.Benchmark
benchTake text name
    = C.bench name
    $ C.nf (\x -> foldr S.take x [1000, 999 .. 1 :: Int64]) text

benchSplitAt text name
    = C.bench name
    $ C.nf (\x -> foldr ((fst .) . S.splitAt) x [1000, 999 .. 1 :: Int64]) text

main :: IO ()
main = C.defaultMain
    [ benchCons longText "cons long"
    , benchCons wideText "cons wide"
    , benchSnoc longText "snoc long"
    , benchSnoc wideText "snoc wide"
    , benchLines longYiString "lines long"
    , benchLines wideYiString "lines wide"
    , benchDrop longYiString "drop long"
    , benchDrop wideYiString "drop wide"
    , benchTake longYiString "take long"
    , benchTake wideYiString "take wide"
    , benchSplitAt longYiString "splitAt long"
    , benchSplitAt wideYiString "splitAt wide"
    ]

longText :: String
longText = force . unlines
         $ replicate 1000 "Lorem ipsum dolor sit amet"
{-# NOINLINE longText #-}

longYiString :: S.YiString
longYiString = force (S.fromString longText)
{-# NOINLINE longYiString #-}

wideText :: String
wideText = force . unlines
         $ replicate 10 . concat
         $ replicate 100 "Lorem ipsum dolor sit amet "
{-# NOINLINE wideText #-}

wideYiString :: S.YiString
wideYiString = force (S.fromString wideText)
{-# NOINLINE wideYiString #-}

