{-# LANGUAGE FlexibleContexts #-}

module Adventures where


import Control.Monad
--import System
import System.FilePath
import Text.ParserCombinators.Parsec
import Text.Parsec
  

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
  let hexStr2Int = Prelude.read . ("0x" ++)
  in do
    start <- many1 hexDigit
    char '-'
    end <- many1 hexDigit
    return $ Address (hexStr2Int start) (hexStr2Int end)

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
  let hexStr2Int = Prelude.read . ("0x" ++)
  in do
    maj <- many1 digit
    char ':'
    min <- many1 digit
    return $ Device (hexStr2Int maj) (hexStr2Int min)

parseRegion :: Stream s m Char => ParsecT s u m MemRegion
parseRegion =
  let hexStr2Int = Prelude.read . ("0x" ++)
      parsePath = (many1 $ char ' ') >> (many1 $ anyChar)
  in do
    addr <- parseAddress
    char ' '
    perm <- parsePerms
    char ' '
    off <- many1 hexDigit
    char ' '
    dev <- parseDevice
    char ' '
    ino <- many1 digit
    char ' '
    pat <- parsePath <|> string ""
    return MemRegion {address = addr, perms = perm, offset = hexStr2Int off, device = dev, inode = Prelude.read ino, pathname = pat}