module BlankSpec where

import Test.Hspec
import Main

spec :: Spec
spec = do
  describe "blank test" $ do
    it "test" $ 3 `shouldBe` (3 :: Int)
