module RealWorldExampleSpec where

import RealWorldExample
import Text.Parsec
import Test.Hspec


spec :: Spec
spec = 
    describe "parsing CSV" $ 
        it "works with simple example" $ 
            parseCSV "field one,field two,field three\nsecond,line\n" `shouldBe` Right [["field one","field two","field three"],["second","line"]]
