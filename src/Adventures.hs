{-# LANGUAGE FlexibleContexts #-}

module Adventures where


import Control.Monad
--import System
import System.FilePath
import Text.ParserCombinators.Parsec
import Text.Parsec
  

data Address = Address { start :: Integer, end :: Integer}
  deriving Show

data Access = Shared | Private
  deriving Show

data Perms = Perms {
    read       :: Bool,
    write      :: Bool,
    executable :: Bool,
    access     :: Access
} deriving Show

data Device = Device { major:: Integer, minor :: Integer}
  deriving Show

data MemRegion = MemRegion {
    address  :: Address,
    perms    :: Perms,
    offset   :: Integer,
    device   :: Device,
    inode    :: Integer,
    pathname :: String 
} deriving Show


parseAddress :: Stream s m Char => ParsecT s u m Address
parseAddress = let
    hexStr2Int = Prelude.read . ("0x" ++)
  in do
    start <- many1 hexDigit
    char '-'
    end <- many1 hexDigit
    return $ Address (hexStr2Int start) (hexStr2Int end)
