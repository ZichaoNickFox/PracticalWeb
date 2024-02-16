module Main where

import Test.Hspec
import Domain.Validation
import           Safe
import           Text.Regex.PCRE.Heavy
import           Text.Regex.PCRE.Light
import Data.Text
import Domain.Registration

-- ebook p59

validationSpec :: SpecWith ()
validationSpec = do
  it "lengthBetween" $ lengthBetween 1 5 "err" "12345" `shouldBe` Nothing
  it "lengthBetween" $ lengthBetween 1 5 "err" "123456" `shouldBe` Just "err"
  it "checking" $ ("https://val.packett.cool" :: Text) =~ [re|^http.*|] `shouldBe` True
  it "regex" $ regexMatches [re|^hello|] "err" "hello world" `shouldBe` Nothing
  it "regex" $ regexMatches [re|^hello|] "err" "failed world" `shouldBe` Just "err"
  let containsA = regexMatches [re|A|] "errA"
  let containsB = regexMatches [re|B|] "errB"
  it "validate" $ validate id [containsA, containsB] "abc" `shouldBe` Left ["errA", "errB"]
  it "validate" $ validate id [containsA, containsB] "ABc" `shouldBe` Right "ABc"

registrationSpec :: SpecWith ()
registrationSpec = do
  it "register" $ do
    let Right email = mkEmail "test@example.com"
    let Right password = mkPassword "123456"
    let auth = Auth email password
    r <- register auth
    r `shouldBe` Right ()

spec :: SpecWith ()
spec = do
  describe "validationSpec" validationSpec
  describe "registrationSpec" registrationSpec

main :: IO ()
main = hspec spec
