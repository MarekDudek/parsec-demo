{-# LANGUAGE FlexibleContexts #-}

module Adventures where

import Control.Monad
--import System
import System.FilePath
import Text.ParserCombinators.Parsec
import Text.Parsec
import Data.Text
import Data.Text.Read

data Address = Address { start :: Integer, end :: Integer}
  deriving (Eq, Show)

data Access = Shared | Private
  deriving (Eq, Show)

data Perms = Perms {
    read2       :: Bool,
    write      :: Bool,
    executable :: Bool,
    access     :: Access
} deriving (Eq, Show)

data Device = Device { major:: Integer, minor :: Integer}
  deriving (Eq, Show)

data MemRegion = MemRegion {
    address  :: Address,
    perms    :: Perms,
    offset   :: Integer,
    device   :: Device,
    inode    :: Integer,
    pathname :: String 
} deriving (Eq, Show)


addressParser :: Stream s m Char => ParsecT s u m Address
addressParser = 
  do
    s <- hexadec
    char '-'
    e <- hexadec
    return Address { start = s, end = e }

accessParser :: Stream s m Char => ParsecT s u m Access
accessParser = 
  let private = do char 'p'
                   return Private
      shared  = do char 's' 
                   return Shared
  in private <|> shared
    
permissionsParser :: Stream s m Char => ParsecT s u m Perms
permissionsParser = 
  do
    r <- anyChar
    w <- anyChar
    x <- anyChar
    a <- accessParser
    return Perms { read2 = r =='r', write = w == 'w', executable = x == 'x', access = a }

deviceParser :: Stream s m Char => ParsecT s u m Device
deviceParser = 
  do
    m1 <- hexadec
    char ':'
    m2 <- hexadec
    return Device { major = m1, minor = m2 }

regionParser :: Stream s m Char => ParsecT s u m MemRegion
regionParser =
  let parsePath = many1 (char ' ') >> many1 anyChar
  in do
    a <- addressParser
    char ' '
    p <- permissionsParser
    char ' '
    o <- hexadec
    char ' '
    d <- deviceParser
    char ' '
    i <- many1 digit
    char ' '
    pn <- parsePath <|> string ""
    return MemRegion { address = a, perms = p, offset = o, device = d, inode = Prelude.read i, pathname = pn }


hexadec :: Stream s m Char => ParsecT s u m Integer
hexadec = 
  do digits <- many1 hexDigit
     let p = pack digits
         Right (h, _) = hexadecimal p
     return h
