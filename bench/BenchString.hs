{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Criterion.Main as C
import Control.DeepSeq
import Data.List (foldl')
import qualified Y.String as S

benchCons :: String -> String -> C.Benchmark
benchCons text name = C.bench name $ C.nf go ""
    where go start = foldr S.cons start text

benchSnoc :: String -> String -> C.Benchmark
benchSnoc text name = C.bench name$ C.nf go ""
    where go start = foldl' S.snoc start text

benchLength :: S.YiString -> String -> C.Benchmark
benchLength text name = C.bench name
            $ C.nf S.length text

benchLines :: S.YiString -> String -> C.Benchmark
benchLines text name = C.bench name
           $ C.nf (S.splitOnNewLines :: S.YiString -> [S.YiString]) text

main :: IO ()
main = C.defaultMain
    -- [ benchCons longText "cons long"
    -- , benchCons wideText "cons wide"
    -- , benchSnoc longText "snoc long"
    -- , benchSnoc wideText "snoc wide"
    -- ,
    [ benchLength longYiString "length long"
    , benchLength wideYiString "length wide"
    , benchLines longYiString "lines long"
    , benchLines wideYiString "lines wide"
    ]

longText :: String
longText = force . unlines
         $ replicate 3000 "Lorem ipsum dolor sit amet"
{-# NOINLINE longText #-}

longYiString :: S.YiString
longYiString = force (S.fromString longText)
{-# NOINLINE longYiString #-}

wideText :: String
wideText = force . unlines
         $ replicate 20 . concat
         $ replicate 150 "Lorem ipsum dolor sit amet "
{-# NOINLINE wideText #-}

wideYiString :: S.YiString
wideYiString = force (S.fromString wideText)
{-# NOINLINE wideYiString #-}

