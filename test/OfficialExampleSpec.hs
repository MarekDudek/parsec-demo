module OfficialExampleSpec where


import OfficialExample
import Text.Parsec
import Test.Hspec


spec :: Spec
spec = 
    describe "parsing parenthesis" $ do
        it "deals with single pair" $ do
            parse parens "" "()" `shouldBe` Right ()
        it "deals with more complicated example" $ do
            parse parens "" "()(())" `shouldBe` Right ()
        xit "properly reports unmatched parentheses" $ do
            parse parens "" "(" `shouldBe` undefined


main :: IO ()
main = hspec spec
