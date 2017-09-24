module RandomExamplesSpec where


import RandomExamples
import MessageHelper
import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos
import Test.Hspec
    

spec :: Spec
spec = 
    describe "Random examples" $ do
        it "exactly eight letters works" $
            parse eightLetters "" "abcdefgh" `shouldBe` Right "abcdefgh"
        it "more than eight letters works" $
            parse eightLetters "" "abcdefghi" `shouldBe` Right "abcdefgh"      
        it "less than eight letters works" $
            parse eightLetters "" "abcdefg" `shouldBe` Left (newErrorMessage (SysUnExpect "") (newPos "" 1 8))
