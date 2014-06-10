module Y.Keymap.Emacs
    ( emacsKeymap
    ) where

import Y.Keymap (Binding, Keymap, mkStatelessKeymap)

emacsKeymap :: Keymap
emacsKeymap = mkStatelessKeymap bindings

bindings :: [Binding]
bindings = []
