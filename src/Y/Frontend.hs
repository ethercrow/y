module Y.Frontend where

import Y.String
import qualified FRP.Sodium as Sodium

data InputOccurence = KChar Char | KEsc
    deriving (Show)

data ViewModel = ViewModel YiString
    deriving Show

data Frontend = Frontend
    { inputEvent :: IO (Sodium.Event InputOccurence)
    , render :: ViewModel -> IO ()
    }
