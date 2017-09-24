module MessageHelper where

import Text.Parsec.Error
import Text.Parsec.Pos

instance Show Message where
    show (SysUnExpect s) = show s
    show (UnExpect s)    = show s
    show (Expect s)      = show s
    show (Message s)     = show s