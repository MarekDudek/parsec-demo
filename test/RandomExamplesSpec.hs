module RandomExamplesSpec where


import RandomExamples
import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos
import Test.Hspec
    

instance Show Message where
    show (SysUnExpect s) = show s
    show (UnExpect s)    = show s
    show (Expect s)      = show s
    show (Message s)     = show s

spec :: Spec
spec = 
    describe "Random examples" $ do
        it "exactly eight letters works" $
            parse eightLetters "" "abcdefgh" `shouldBe` Right "abcdefgh"
        it "more than eight letters works" $
            parse eightLetters "" "abcdefghi" `shouldBe` Right "abcdefgh"      
        it "less than eight letters works" $
            parse eightLetters "" "abcdefg" `shouldBe` Left (newErrorMessage (SysUnExpect "") (newPos "" 1 8))
