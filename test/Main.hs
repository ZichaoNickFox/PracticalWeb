module Main where

import Test.Hspec
import Domain.Validation
import           Safe
import           Test.Hspec
import           Text.Regex.PCRE.Heavy
import           Text.Regex.PCRE.Light

spec :: SpecWith ()
spec = do
  it "lengthBetween" $ lengthBetween 1 5 "err" "12345" `shouldBe` Nothing
  it "lengthBetween" $ lengthBetween 1 5 "err" "123456" `shouldBe` Just "err"
  it "checking" $ "https://val.packett.cool" =~ [re|^http.*|] `shouldBe` True

main :: IO ()
main = hspec spec
