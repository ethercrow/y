{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
    , takeScreenful
    , cons, snoc
    , splitAt
    , splitAtLine
    , splitOnNewLines
    , countNewLines
    , insertAt
    , deleteAt
    , readFile, writeFile
    ) where

import Prelude hiding (null, length, concat, splitAt, reverse, take, drop, lines
    , foldr, foldl
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
    = ShortLine TL.Text !Size
    | LongLine (S.Seq TL.Text) !Size
    deriving Show

data YiString = YiString
    { fromYiString :: S.Seq Line
    , stringSize :: !Size
    } deriving Show

mkLine :: TL.Text -> Line
mkLine t = mkLine' t (Size (TL.length t))

mkLine' :: TL.Text -> Size -> Line
mkLine' t (Size n) | n < maxShortLineLength = ShortLine t (Size n)
mkLine' t size = LongLine (S.fromList $ map TL.fromStrict $ TL.toChunks t) size

instance Monoid Line where
    mempty = ShortLine "" (Size 0)
    mappend (ShortLine l (Size lsize)) (ShortLine r (Size rsize))
        | lsize + rsize <= maxShortLineLength = ShortLine (l <> r) (Size (lsize + rsize))
    mappend (ShortLine l lsize) (ShortLine r rsize)
        = LongLine (S.fromList [l, r]) (lsize <> rsize)
    mappend (ShortLine l lsize) (LongLine rs rsize) = LongLine (l <| rs) (lsize <> rsize)
    mappend (LongLine ls lsize) (ShortLine r rsize) = LongLine (ls |> r) (lsize <> rsize)
    mappend (LongLine ls lsize) (LongLine rs rsize) = LongLine (ls <> rs) (lsize <> rsize)

instance NFData Line where
    rnf (ShortLine t _) = rnf t
    rnf (LongLine chunks _) = rnf chunks

lineToLazyText :: Line -> TL.Text
lineToLazyText (ShortLine t _) = t
lineToLazyText (LongLine chunks _) = foldr mappend "" chunks

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
singleton c = YiString (S.singleton (ShortLine (TL.singleton c) (Size 1))) (Size 1)

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

findSplitBoundary :: Int64 -> S.Seq Line -> (Int64, Int)
findSplitBoundary n64 = go 0 0 . toList
    where go !lengthAcc !index [] = (lengthAcc, index)
          go !lengthAcc !index (l:_)
              | lengthAcc + 1 + fromSize (lineSize l) > n64 = (lengthAcc, index)
          go !lengthAcc !index (l:ls)
              = go (lengthAcc + 1 + fromSize (lineSize l)) (succ index) ls
          toList = foldr (:) [] . S.viewr

splitAt :: Int -> YiString -> (YiString, YiString)
splitAt n s | n <= 0 = (mempty, s)
splitAt n s@(YiString _lines (Size size)) | fromIntegral n >= size = (s, mempty)
splitAt n (YiString lines (Size size)) =
    (YiString leftLines (Size n64), YiString rightLines (Size (size - n64)))
    where n64 = fromIntegral n :: Int64
          (positionAtStartOfBoundaryLine, boundaryLineIndex) = findSplitBoundary n64 lines
          mostlyLeftPart = S.take (succ boundaryLineIndex) lines
          strictlyRightPart = S.drop (succ boundaryLineIndex) lines
          strictlyLeftPart S.:> lastLeftLine
              = S.viewr mostlyLeftPart
          (leftLines, rightLines)
              = (strictlyLeftPart
                    |> lineTake (n64 - positionAtStartOfBoundaryLine) lastLeftLine,
                 lineDrop (n64 - positionAtStartOfBoundaryLine) lastLeftLine
                    <| strictlyRightPart)

splitAtLine :: Int -> YiString -> (YiString, YiString)
splitAtLine 0 s = (mempty, s)
splitAtLine i s@(YiString lines _) | i >= S.length lines = (s, mempty)
splitAtLine i (YiString lines _)
    = ( YiString ls' (Size (fromIntegral i) <> foldMap lineSize ls')
      , YiString rs (Size (fromIntegral (S.length rs - 1)) <> foldMap lineSize rs)
      )
    where ls = S.take i lines
          rs = S.drop i lines
          ls' = if S.length rs >= 1 || lineSize (ls ^. _last) > Size 0
                then ls |> mempty
                else ls

splitOnNewLines :: (Applicative f, Monoid (f YiString)) => YiString -> f YiString
splitOnNewLines (YiString lines _) = foldMap go lines
    where go line = pure (YiString (S.singleton line) (lineSize line))

countNewLines :: YiString -> Int
countNewLines = pred . fromIntegral . S.length . fromYiString

reverseLine :: Line -> Line
reverseLine (ShortLine t size) = ShortLine (TL.reverse t) size
reverseLine (LongLine chunks size) = LongLine (fmap TL.reverse (S.reverse chunks)) size

reverse :: YiString -> YiString
reverse (YiString lines size) = YiString (fmap reverseLine $ S.reverse lines) size

take :: Integral i => i -> YiString -> YiString
take n = fst . splitAt (fromIntegral n)

takeScreenful :: Int -> Int -> YiString -> YiString
takeScreenful w h (YiString _lines (Size size)) | w == 0 || h == 0 || size == 0 = mempty
takeScreenful w h (YiString lines (Size size)) =
    if headLineLength >= w * h
    then YiString (S.singleton (lineTake (w * h) headLine)) (Size (fromIntegral (w * h)))
    else if h - headLineHeight > 0
    then YiString (S.fromList [headLine, mempty]) (Size (fromIntegral (succ headLineLength)))
         <> takeScreenful w (h - headLineHeight) tailString
    else YiString (S.singleton headLine) headLineSize
    where
    headLineHeight = max 1 (headLineLength `div` w
                             + if headLineLength `rem` w > 1 then 1 else 0)
    headLineLength = fromIntegral $ fromSize headLineSize
    headLineSize = lineSize headLine
    (headLine, tailString) = case S.viewl lines of
        l S.:< tailLines
            -> (l, YiString tailLines (Size (size - 1 - fromIntegral headLineLength)))

lineDrop :: Integral i => i -> Line -> Line
lineDrop 0 l = l
lineDrop n l | fromIntegral n >= fromSize (lineSize l) = mempty
lineDrop n (ShortLine t (Size size))
    = ShortLine (TL.drop (fromIntegral n) t) (Size (size - fromIntegral n))
lineDrop n l@(LongLine _chunks (Size size)) | size - fromIntegral n < maxShortLineLength
    = ShortLine (TL.drop (fromIntegral n) (lineToLazyText l)) (Size (size - fromIntegral n))
lineDrop n l@(LongLine _chunks (Size size))
    = mkLine' (TL.drop (fromIntegral n) (lineToLazyText l)) (Size (size - fromIntegral n))

lineTake :: Integral i => i -> Line -> Line
lineTake 0 _ = mempty
lineTake n l | fromSize (lineSize l) < fromIntegral n = l
lineTake n l = mkLine' (TL.take (fromIntegral n) (lineToLazyText l)) (Size (fromIntegral n))

drop :: Integral i => i -> YiString -> YiString
drop n = snd . splitAt (fromIntegral n)

lineSnoc :: Line -> Char -> Line
lineSnoc (ShortLine t (Size size)) c | size > maxShortLineLength
    = LongLine (S.fromList [t, TL.singleton c]) (Size (succ size))
lineSnoc (ShortLine t (Size size)) c
    = ShortLine (t `TL.snoc` c) (Size (succ size))
lineSnoc (LongLine chunks (Size size)) c | TL.length (chunks ^. _last) >= maxShortLineLength
    = LongLine (chunks |> TL.singleton c) (Size (succ size))
lineSnoc (LongLine chunks (Size size)) c
    = LongLine (chunks & over _last (`TL.snoc` c)) (Size (succ size))

lineCons :: Char -> Line -> Line
lineCons c (ShortLine t (Size size)) | size > maxShortLineLength
    = LongLine (S.fromList [TL.singleton c, t]) (Size (succ size))
lineCons c (ShortLine t (Size size))
    = ShortLine (c `TL.cons` t) (Size (succ size))
lineCons c (LongLine chunks (Size size))
    | TL.length (chunks ^. _head) >= maxShortLineLength
    = LongLine (TL.singleton c <| chunks) (Size (succ size))
lineCons c (LongLine chunks (Size size))
    = LongLine (chunks & over _head (c `TL.cons`)) (Size (succ size))

lineSize :: Line -> Size
lineSize (ShortLine _t size) = size
lineSize (LongLine _chunks size) = size

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
