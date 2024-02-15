module Spec where

import Test.Hspec
import Domain.Validation

spec :: SpecWith ()
spec = do
  it "lengthBetween" $ lengthBetween 1 5 "err" "12345" `shouldBe` Nothing
  it "lengthBetween" $ lengthBetween 1 5 "err" "123456" `shouldBe` Just "err"

main :: IO ()
main = spec
