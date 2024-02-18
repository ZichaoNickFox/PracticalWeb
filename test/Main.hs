module Main where

import           Data.Text
import           Debug.Trace           (traceShow)
import           Domain.Registration
import           Domain.Validation
import           Test.Hspec
import           Text.Regex.PCRE.Heavy

shouldBeWhat :: (Show a) =>  a -> () -> IO ()
shouldBeWhat a () = traceShow a (return ())

-- ebook p59
domainSpec :: SpecWith ()
domainSpec = do
  it "lengthBetween" $ lengthBetween 1 5 "err" "12345" `shouldBe` Nothing
  it "lengthBetween" $ lengthBetween 1 5 "err" "123456" `shouldBe` Just "err"
  it "checking" $ ("https://val.packett.cool" :: Text) =~ [re|^http.*|] `shouldBe` True
  it "regex" $ regexMatches [re|^hello|] "err" "hello world" `shouldBe` Nothing
  it "regex" $ regexMatches [re|^hello|] "err" "failed world" `shouldBe` Just "err"
  let containsA = regexMatches [re|A|] "errA"
  let containsB = regexMatches [re|B|] "errB"
  it "validate" $ validate id [containsA, containsB] "abc" `shouldBe` Left ["errA", "errB"]
  it "validate" $ validate id [containsA, containsB] "ABc" `shouldBe` Right "ABc"
  -- it "register" $ do
  --   let Right email = mkEmail "test@example.com"
  --   let Right password = mkPassword "123456"
  --   let auth = Auth email password
  --   r <- register auth
  --   r `shouldBe` Right ()

-- ebook 81
-- adapterInMemorySpec :: SpecWith ()
-- adapterInMemorySpec = do
--   let Right email = mkEmail "example@test.com"
--   let Right password = mkPassword "123456ABCDefgh"
--   let auth = Auth email password
--   it "addAuth" $ do
--     A.addAuth auth `shouldBeWhat` ()

spec :: SpecWith ()
spec = do
  describe "domainSpec" domainSpec
  -- describe "adapterInMemorySpec" adapterInMemorySpec

main :: IO ()
main = hspec spec
