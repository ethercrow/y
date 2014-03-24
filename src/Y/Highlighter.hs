module Y.Highlighter where

import Y.Common
import Y.String

newtype Highlighter = Highlighter (YiString -> IO BufferOverlay)
