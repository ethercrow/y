{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Y.String
    ( YiString
    , Position(..)
    , Size(..)

    , fromString, toString
    , singleton, null, empty, length
    , append, concat
    , reverse
    , take, drop
    , cons, snoc
    , splitAt, splitAtLine
    , countNewLines

    ) where

import Prelude hiding (null, length, concat, splitAt, reverse, take, drop, lines)

import Control.Lens hiding (cons, snoc)
import Data.Int
import qualified Data.List as L
import Data.Monoid
import Data.String
import qualified Data.Text.Lazy as TL

newtype YiString = YiString { fromYiString :: TL.Text }
    deriving (Monoid, Show, Eq)

instance (t ~ YiString) => Rewrapped YiString TL.Text
instance Wrapped YiString where
    type Unwrapped YiString = TL.Text
    _Wrapped' = iso fromYiString YiString
    {-# INLINE _Wrapped' #-}

instance IsString YiString where
    fromString = YiString . TL.pack

toString :: YiString -> String
toString (YiString t) = TL.unpack t

-- | Position measured in characters, not bytes.
newtype Position = Position Int
    deriving (Eq, Show, Ord)

-- | Size measured in characters, not bytes.
newtype Size = Size Int64
    deriving (Eq, Show, Ord)

singleton :: Char -> YiString
singleton = YiString . TL.singleton

null :: YiString -> Bool
null (YiString t) = TL.null t

empty :: YiString
empty = mempty

append :: YiString -> YiString -> YiString
append = mappend

concat :: [YiString] -> YiString
concat = mconcat

length :: YiString -> Size
length (YiString t) = Size (TL.length t)

splitAt :: Int -> YiString -> (YiString, YiString)
splitAt i (YiString t) = over both YiString (TL.splitAt (fromIntegral i) t)

splitAtLine :: Int -> YiString -> (YiString, YiString)
splitAtLine 0 s = (mempty, s)
splitAtLine i (YiString t)
    = case lines of
        [l] -> (YiString l, mempty)
        ls | L.length ls <= i -> (YiString t, mempty)
        ls -> (L.take i ls, L.drop i ls)
            & over both (TL.intercalate "\n")
            & over _1 (\l -> if "\n" `TL.isSuffixOf` l then l else l <> "\n")
            & over both YiString
    where lines = TL.splitOn "\n" t

countNewLines :: YiString -> Int
countNewLines = fromIntegral . TL.count "\n" . fromYiString

reverse :: YiString -> YiString
reverse = YiString . TL.reverse . fromYiString

take :: Integral i => i -> YiString -> YiString
take n = YiString . TL.take (fromIntegral n) . fromYiString

drop :: Integral i => i -> YiString -> YiString
drop n = YiString . TL.drop (fromIntegral n) . fromYiString

cons :: Char -> YiString -> YiString
cons c = YiString . TL.cons c . fromYiString

snoc :: YiString -> Char -> YiString
snoc s c = YiString (TL.snoc (fromYiString s) c)
