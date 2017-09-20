{-# LANGUAGE FlexibleContexts #-}

module OfficialExample where

import Text.Parsec

parenSet :: Stream s m Char => ParsecT s u m Char
parenSet = char '(' >> many parenSet >> char ')'

parens :: Stream s m Char => ParsecT s u m ()
parens = (many parenSet >> eof) <|> eof


--t3 = parse parens "" "("
