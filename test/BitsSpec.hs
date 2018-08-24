module BitsSpec where

import           Test.Hspec

import           Bits

spec :: Spec
spec =
  describe "OTP" $
    it "is symmetric" $ do
      let p = "hello"
          kBool = bitsToBooleans (Bits "1001001")
          pBools = map bitsToBooleans $ map charToBits p
          c = map bitsToChar $ map booleansToBits (otp kBool pBools)
          cBools = map bitsToBooleans $ map charToBits c
          p' = map bitsToChar $ map booleansToBits (otp kBool cBools)
      p `shouldBe` p'
