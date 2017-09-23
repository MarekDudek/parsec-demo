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
    read       :: Bool,
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


parseAddress :: Stream s m Char => ParsecT s u m Address
parseAddress = 
  do
    s <- hexadec
    char '-'
    e <- hexadec
    return Address { start = s, end = e }

parsePerms :: Stream s m Char => ParsecT s u m Perms
parsePerms = 
  let cA a = case a of 
        'p' -> Private
        's' -> Shared
  in do
    r <- anyChar
    w <- anyChar
    x <- anyChar
    a <- anyChar
    return $ Perms (r =='r') (w == 'w') (x == 'x') (cA a)

parseDevice :: Stream s m Char => ParsecT s u m Device
parseDevice = 
  do
    major <- hexadec
    char ':'
    minor <- hexadec
    return Device { major = major, minor = minor }

parseRegion :: Stream s m Char => ParsecT s u m MemRegion
parseRegion =
  let parsePath = many1 (char ' ') >> many1  anyChar
  in do
    addr <- parseAddress
    char ' '
    perm <- parsePerms
    char ' '
    off <- hexadec
    char ' '
    dev <- parseDevice
    char ' '
    ino <- many1 digit
    char ' '
    pat <- parsePath <|> string ""
    return MemRegion {address = addr, perms = perm, offset = off, device = dev, inode = Prelude.read ino, pathname = pat}


hexadec :: Stream s m Char => ParsecT s u m Integer
hexadec = 
  do digits <- many1 hexDigit
     let p = pack digits
         Right (h, _) = hexadecimal p
     return h
