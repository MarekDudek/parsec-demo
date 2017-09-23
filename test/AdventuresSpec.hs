module AdventuresSpec where

import Adventures
import Text.Parsec
import Text.ParserCombinators.Parsec
import Test.Hspec
import Prelude hiding (read)

spec :: Spec
spec = 
    describe "Parsing PID maps" $ do
        describe "Parsing address" $ do
            it "works on simple case" $ 
                parse parseAddress "" "0-1" `shouldBe` Right Address {start = 0, end = 1}
            it "properly parses hex digits" $
                parse parseAddress "hhh" "01234567-89abcdef" `shouldBe` Right Address {start = 19088743, end = 2309737967}
        describe "Parsing permissions" $ 
            it "works in only case" $
                parse parsePerms "" "r-xp" `shouldBe` Right Perms {read = True, write = False, executable = True, access = Private}
        describe "Device parsing" $ do
            it "works on simple case" $
                parse parseDevice "" "0:1" `shouldBe` Right Device {major = 0, minor = 1}
            it "works on real case" $
                parse parseDevice "" "03:0c" `shouldBe` Right Device {major = 3, minor = 12 }
        describe "Memory region parsing" $ 
            it "works on example" $
                parse parseRegion "" "08058000-0805b000 rwxp 00000000 00:00 0 " `shouldBe` Right region
                where region = MemRegion {address = a, perms = p, offset = o, device = d, inode = i, pathname = pn}
                      a = Address {start = 134578176, end = 134590464}
                      p = Perms {read = True, write = True, executable = True, access = Private}
                      o = 0
                      d = Device {major = 0, minor = 0}
                      i = 0
                      pn = ""
