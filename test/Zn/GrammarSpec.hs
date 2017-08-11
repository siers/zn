module Zn.GrammarSpec (spec) where

import Test.Hspec
import Zn.Grammar

spec :: Spec
spec = do
  describe "addressing" $ do
    it "addressable via !" $
      matches (addressed "nick") "!cmd" `shouldBe` Just "cmd"

    it "addressable via nick" $
      matches (addressed "nick") "nick, cmd" `shouldBe` Just "cmd"

    it "addressable with #()" $
      matches (addressed "nick") "#(cmd)" `shouldBe` Just "cmd"

    it "addressable with #() and escapable" $
      matches (addressed "nick") "text #(\\)) more text" `shouldBe` Just ")"

    it "addressable only once" $
      matches (addressed "nick") "text #(foo) more #(bar)" `shouldBe` Just "foo"
