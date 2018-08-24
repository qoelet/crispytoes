module BooleanSpec where

import           Test.Hspec

import           Boolean

spec :: Spec
spec = do
  describe "bAnd" $
    it "ands" $ do
      bAnd T T `shouldBe` T
      bAnd F T `shouldBe` F
      bAnd T F `shouldBe` F
      bAnd F F `shouldBe` F
  describe "bOr" $
    it "ors" $ do
      bOr T T `shouldBe` T
      bOr F T `shouldBe` T
      bOr T F `shouldBe` T
      bOr F F `shouldBe` F
  describe "bNot" $
    it "nots" $ do
      bNot T `shouldBe` F
      bNot F `shouldBe` T
  describe "bXor" $
    it "xors" $ do
      bXor T F `shouldBe` T
      bXor F T `shouldBe` T
      bXor T T `shouldBe` F
      bXor F F `shouldBe` F
  describe "bEquiv" $
    it "equivs" $ do
      bEquiv T T `shouldBe` T
      bEquiv F F `shouldBe` T
      bEquiv F T `shouldBe` F
      bEquiv T F `shouldBe` F
  describe "bImplies" $
    it "implies" $ do
      bImplies T F `shouldBe` F
      bImplies F F `shouldBe` T
      bImplies T T `shouldBe` T
      bImplies F T `shouldBe` T
  describe "bNand" $
    it "nands" $ do
      bNand T F `shouldBe` T
      bNand F T `shouldBe` T
      bNand F F `shouldBe` T
      bNand T T `shouldBe` F
  describe "bNor" $
    it "nors" $ do
      bNor F F `shouldBe` T
      bNor F T `shouldBe` F
      bNor T F `shouldBe` F
      bNor T T `shouldBe` F
