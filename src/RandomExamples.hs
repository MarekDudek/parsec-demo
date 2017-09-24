{-# LANGUAGE FlexibleContexts #-}

module RandomExamples where

import Text.ParserCombinators.Parsec
import Text.Parsec


eightLetters :: Stream s m Char => ParsecT s u m String
eightLetters =
    count 8 anyChar
        