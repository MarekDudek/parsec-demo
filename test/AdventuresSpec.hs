module AdventuresSpec where

import Adventures
import Text.Parsec
import Text.ParserCombinators.Parsec
import Test.Hspec

spec :: Spec
spec = 
    describe "Parsing PID maps" $ 
        describe "Parsing address" $ do
            it "works on simple case" $ 
                parse parseAddress "" "0-1" `shouldBe` Right Address {start = 0, end = 1}
            it "properly parses hex digits" $
                parse parseAddress "hhh" "01234567-89abcdef" `shouldBe` Right Address {start = 19088743, end = 2309737967}
