{-# LANGUAGE OverloadedStrings #-}
module Y.Highlighter.Kate
    ( kateHighlighter
    ) where

import qualified Data.Vector as V

import Y.Common
import Y.Highlighter
import qualified Text.Highlighting.Kate as K
import qualified Y.String as S

kateHighlighter :: S.YiString -> Maybe Highlighter
kateHighlighter language | S.toString language `notElem` K.languages = Nothing
kateHighlighter language = Just . Highlighter $ \s -> do
    let sourceLines = K.highlightAs (S.toString language) (S.toString s)
        lineColors = V.map (LineOverlay . lineToColor) $ V.fromList sourceLines
        lineToColor [] = Default
        lineToColor ((token, _) : _) = tokenToColor token
        tokenToColor token | token `elem` greenTokens = Green
        tokenToColor token | token `elem` redTokens = Red
        tokenToColor _ = Default
        greenTokens = [K.KeywordTok, K.DataTypeTok, K.DecValTok, K.BaseNTok, K.FloatTok]
        redTokens = [K.CharTok, K.StringTok, K.CommentTok, K.FunctionTok]
    return $! BufferOverlay "kateSyntax" lineColors

