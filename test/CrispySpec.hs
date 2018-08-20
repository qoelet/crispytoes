module CrispySpec where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Crispy

spec :: Spec
spec = do
  describe "Caesar" $
    it "is symmetric" $ monadicIO $ do
      foo <- run (P . wrapMod <$> generate arbitrary :: IO P)
      k <- run (wrapMod <$> generate arbitrary :: IO Int)
      assert $
        decryptUsing (unshift k) (encryptUsing (shift k) foo) == foo
  describe "Digraphs" $
    it "foo" $ do
      let foo = P 1
          bar = P 3
          pair = Di foo bar
          expected = P 29
      group pair `shouldBe` P 29

wrapMod :: Int -> Int
wrapMod i
  | i < 0 = wrapMod (i + 26)
  | i >= 26 = i `mod` 26
  | otherwise = i
