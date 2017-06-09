module Zn.GrammarSpec (spec) where

import Test.Hspec
import Zn.Grammar

spec :: Spec
spec = do
  describe "grammar" $ do
    it "works" $ do
      1 `shouldBe` 1
