{-# LANGUAGE OverloadedStrings #-}

module Y.String
    ( YiString
    , Position(..)
    , fromString, toString
    , toReverseString
    , fromLazyText, toLazyText
    , empty
    , singleton, null, length
    , append, concat
    , reverse
    , take, drop
    , cons, snoc
    , splitAt
    , splitAtLine
    , splitOnNewLines
    , countNewLines
    , insertAt
    , deleteAt
    , readFile, writeFile
    ) where

import Prelude hiding (null, length, concat, splitAt, reverse, take, drop, lines, foldr
    , readFile, writeFile)

import Control.Applicative hiding (empty)
import Control.DeepSeq
import Control.Lens hiding (cons, snoc, index)
import Data.Binary
import Data.Default
import Data.Foldable (foldr, foldMap)
import Data.Int
import Data.Monoid
import qualified Data.Sequence as S
import Data.String hiding (lines)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Encoding as TE

maxShortLineLength :: Int64
maxShortLineLength = 128

data Line
    = ShortLine TL.Text
    | LongLine (S.Seq TL.Text)
    deriving Show

data YiString = YiString
    { fromYiString :: S.Seq Line
    , stringSize :: !Size
    } deriving Show

mkLine :: TL.Text -> Line
mkLine t | TL.length t < maxShortLineLength = ShortLine t
mkLine t = LongLine $ S.fromList $ map TL.fromStrict $ TL.toChunks t

instance Monoid Line where
    mempty = ShortLine ""
    mappend (ShortLine l) (ShortLine r)
        | TL.length l + TL.length r <= maxShortLineLength = ShortLine (l <> r)
    mappend (ShortLine l) (ShortLine r) = LongLine (S.fromList [l, r])
    mappend (ShortLine l) (LongLine rs) = LongLine (l <| rs)
    mappend (LongLine ls) (ShortLine r) = LongLine (ls |> r)
    mappend (LongLine ls) (LongLine rs) = LongLine (ls <> rs)

instance NFData Line where
    rnf (ShortLine t) = rnf t
    rnf (LongLine chunks) = rnf chunks

lineToLazyText :: Line -> TL.Text
lineToLazyText (ShortLine t) = t
lineToLazyText (LongLine chunks) = foldr mappend "" chunks

instance Monoid YiString where
    mempty = ""
    mappend s (YiString _ (Size 0)) = s
    mappend (YiString _ (Size 0)) s = s
    mappend (YiString l sl) (YiString r sr)
        = YiString ((l' S.|> (lend <> rbegin)) <> r') (sl <> sr)
        where l' S.:> lend = S.viewr l
              rbegin S.:< r' = S.viewl r

fromLazyText :: TL.Text -> YiString
fromLazyText t = YiString (S.fromList $ map mkLine $ TL.splitOn "\n" t)
                          (Size $ TL.length t)

instance IsString YiString where
    fromString = fromLazyText . TL.pack

instance Default YiString where
    def = mempty

instance Eq YiString where
    lhs == rhs
        = stringSize lhs == stringSize rhs
          &&
          toLazyText lhs == toLazyText rhs

instance NFData YiString where
    rnf (YiString lines _) = rnf lines

toLazyText :: YiString -> TL.Text
toLazyText = TL.intercalate "\n"
           . foldr (mappend . return . lineToLazyText) []
           . fromYiString

toString :: YiString -> String
toString = TL.unpack . toLazyText

toReverseString :: YiString -> String
toReverseString = toString . reverse

-- | Position measured in characters, not bytes.
newtype Position = Position Int
    deriving (Eq, Show, Ord)

-- | Size measured in characters, not bytes.
newtype Size = Size
    { fromSize :: Int64
    } deriving (Eq, Show, Ord)

instance Monoid Size where
    mempty = Size 0
    mappend (Size a) (Size b) = Size (a + b)

singleton :: Char -> YiString
singleton '\n' = YiString (S.fromList [mempty, mempty]) (Size 1)
singleton c = YiString (S.singleton $ ShortLine $ TL.singleton c) (Size 1)

null :: YiString -> Bool
null = TL.null . toLazyText

empty :: YiString
empty = mempty

append :: YiString -> YiString -> YiString
append = mappend

concat :: [YiString] -> YiString
concat = mconcat

length :: YiString -> Int
length (YiString _lines (Size size)) = fromIntegral size

splitAt :: Int -> YiString -> (YiString, YiString)
splitAt n s | n <= 0 = (mempty, s)
splitAt n s@(YiString _lines (Size size)) | fromIntegral n >= size = (s, mempty)
splitAt n (YiString lines (Size size)) =
    (YiString leftLines (Size n64), YiString rightLines (Size (size - n64)))
    where n64 = fromIntegral n :: Int64
          cumulativeLengths
              = S.scanl (\acc line -> 1 + acc + fromSize (lineLength line)) 0 lines
          (mostlyLeftPart, strictlyRightPart)
              = S.spanl ((<= n64) . fst) (S.zip cumulativeLengths lines)
          strictlyLeftPart S.:> (x, lastLeftLine)
              = S.viewr mostlyLeftPart
          (leftLines, rightLines)
              = (fmap snd strictlyLeftPart |> lineTake (n64 - x) lastLeftLine,
                 lineDrop (n64 - x) lastLeftLine <| fmap snd strictlyRightPart)

splitAtLine :: Int -> YiString -> (YiString, YiString)
splitAtLine 0 s = (mempty, s)
splitAtLine i s@(YiString lines _) | i >= S.length lines = (s, mempty)
splitAtLine i (YiString lines _)
    = ( YiString ls' (Size (fromIntegral i) <> foldMap lineLength ls')
      , YiString rs (Size (fromIntegral (S.length rs - 1)) <> foldMap lineLength rs)
      )
    where ls = S.take i lines
          rs = S.drop i lines
          ls' = if S.length rs >= 1 || lineLength (ls ^. _last) > Size 0
                then ls |> mempty
                else ls

splitOnNewLines :: (Applicative f, Monoid (f YiString)) => YiString -> f YiString
splitOnNewLines (YiString lines _) = foldMap go lines
    where go line = pure (YiString (S.singleton line) (lineLength line))

countNewLines :: YiString -> Int
countNewLines = pred . fromIntegral . S.length . fromYiString

reverseLine :: Line -> Line
reverseLine (ShortLine t) = ShortLine (TL.reverse t)
reverseLine (LongLine chunks) = LongLine (fmap TL.reverse (S.reverse chunks))

reverse :: YiString -> YiString
reverse (YiString lines size) = YiString (fmap reverseLine $ S.reverse lines) size

take :: Integral i => i -> YiString -> YiString
take n = fromLazyText . TL.take (fromIntegral n) . toLazyText

lineDrop :: Integral i => i -> Line -> Line
lineDrop n = mkLine . TL.drop (fromIntegral n) . lineToLazyText

lineTake :: Integral i => i -> Line -> Line
lineTake n = mkLine . TL.take (fromIntegral n) . lineToLazyText

drop :: Integral i => i -> YiString -> YiString
drop 0 s = s
drop n (YiString _lines (Size size)) | size < fromIntegral n = mempty
drop n (YiString lines (Size size)) =
    let Size firstLength = lineLength (lines ^. _head)
        n64 = fromIntegral n
    in if n64 <= firstLength
       then YiString (lines & over _head (lineDrop n)) (Size (size - n64))
       else drop (n64 - firstLength - 1)
                 (YiString (lines ^. _tail) (Size (size - firstLength - 1)))

lineSnoc :: Line -> Char -> Line
lineSnoc (ShortLine t) c | TL.length t > maxShortLineLength
    = LongLine (S.fromList [t, TL.singleton c])
lineSnoc (ShortLine t) c = ShortLine (t `TL.snoc` c)
lineSnoc (LongLine chunks) c | TL.length (chunks ^. _last) >= maxShortLineLength
    = LongLine (chunks |> TL.singleton c)
lineSnoc (LongLine chunks) c = LongLine (chunks & over _last (`TL.snoc` c))

lineCons :: Char -> Line -> Line
lineCons c (ShortLine t) | TL.length t > maxShortLineLength
    = LongLine (S.fromList [TL.singleton c, t])
lineCons c (ShortLine t) = ShortLine (c `TL.cons` t)
lineCons c (LongLine chunks) | TL.length (chunks ^. _head) >= maxShortLineLength
    = LongLine (TL.singleton c <| chunks)
lineCons c (LongLine chunks) = LongLine (chunks & over _head (c `TL.cons`))

lineLength :: Line -> Size
lineLength (ShortLine t) = Size (TL.length t)
lineLength (LongLine chunks) = foldMap (Size . TL.length) chunks

snoc :: YiString -> Char -> YiString
snoc (YiString lines (Size size)) '\n'
    = YiString (lines |> mempty)
               (Size (succ size))
snoc (YiString lines (Size size)) c
    = YiString (lines & over _last (`lineSnoc` c))
               (Size (succ size))

cons :: Char -> YiString -> YiString
cons '\n' (YiString lines (Size size))
    = YiString (mempty <| lines)
               (Size (succ size))
cons c (YiString lines (Size size))
    = YiString (lines & over _head (c `lineCons`))
               (Size (succ size))

insertAt :: YiString -> Int -> YiString -> YiString
insertAt new index old = oldLeft <> new <> oldRight
    where (oldLeft, oldRight) = splitAt index old

deleteAt :: Int -> Int -> YiString -> YiString
deleteAt index size old = left <> right
    where (left, (_middle, right)) = splitAt size <$> splitAt index old

readFile :: FilePath -> IO YiString
readFile f = fromLazyText <$> TIO.readFile f

writeFile :: FilePath -> YiString -> IO ()
writeFile f = TIO.writeFile f . toLazyText

instance Binary YiString where
    get = fromLazyText . TE.decodeUtf8 <$> get
    put = put . TE.encodeUtf8 . toLazyText
