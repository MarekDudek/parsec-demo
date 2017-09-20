{-# LANGUAGE FlexibleContexts #-}

module OfficialExample where

import Text.Parsec

parenSet :: Stream s m Char => ParsecT s u m Char
parenSet = char '(' >> many parenSet >> char ')'

parens :: Stream s m Char => ParsecT s u m ()
parens = (many parenSet >> eof) <|> eof

t1 = parse parens "" "()" == Right ()
t2 = parse parens "" "()(())" == Right ()
--t3 = parse parens "" "("

official_example_ok = t1 && t2