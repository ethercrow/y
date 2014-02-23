module Main where

import Test.Tasty (defaultMain, testGroup)
import qualified Y.StringTest
import qualified Y.BufferTest

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ Y.StringTest.tests
    , Y.BufferTest.tests
    ]
