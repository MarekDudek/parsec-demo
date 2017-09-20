module RealWorldExampleSpec where

import RealWorldExample
import Text.Parsec
import Test.Hspec


spec :: Spec
spec = 
    describe "parsing CSV" $ 
        it "works with simple example" $ do
            let result = parseCSV "field one,field two,field three\nsecond,line\n"
            result `shouldBe` Right [["field one","field two","field three"],["second","line"]]
