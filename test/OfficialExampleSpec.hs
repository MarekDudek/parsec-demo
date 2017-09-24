module OfficialExampleSpec where


import OfficialExample
import Text.Parsec
import Test.Hspec
import MessageHelper
import Text.Parsec.Error
import Text.Parsec.Pos

spec :: Spec
spec = 
    describe "parsing parenthesis" $ do
        it "deals with single pair" $
            parse parens "" "()" `shouldBe` Right ()
        it "deals with more complicated example" $
            parse parens "" "()(())" `shouldBe` Right ()
        it "properly reports unmatched parentheses" $ do
            let m1 = newErrorMessage (SysUnExpect "") (newPos "" 1 2) 
                m2 = addErrorMessage (SysUnExpect "") m1 
                m3 = addErrorMessage (Expect "\")\"") m2 
                m4 = addErrorMessage (Expect "\"(\"") m3 
            parse parens "" "(" `shouldBe` Left m4


main :: IO ()
main = hspec spec
