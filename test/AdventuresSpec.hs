module AdventuresSpec where

import Adventures
import Text.Parsec
import Test.Hspec

spec :: Spec
spec = 
    describe "parsing address" $ do
        it "works on example" $ do
            pending