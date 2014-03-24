module Y.Common where

import Y.String

data InputOccurrence = KChar Char | KEsc | KEnter
    deriving Show

data ViewModel = ViewModel YiString
    deriving Show

data CoreOutput
    = OutputViewModel ViewModel
    | OutputNoop
    | OutputExit

