module Zn.GrammarSpec (spec) where

import Test.Hspec
import Zn.Grammar

parsesAs match expectation = match `shouldBe` Just expectation

spec :: Spec
spec = do
  describe "addressing" $ do
    it "addressable via !" $
      matches (addressed "nick") "!cmd" `parsesAs` "cmd"

    it "addressable via nick" $
      matches (addressed "nick") "nick, cmd" `parsesAs` "cmd"

    it "addressable with #()" $
      matches (addressed "nick") "#(cmd)" `parsesAs` "cmd"

    it "addressable with #() and escapable" $
      matches (addressed "nick") "text #(\\)) more text" `parsesAs` ")"

    it "addressable only once" $
      matches (addressed "nick") "text #(foo) more #(bar)" `parsesAs` "foo"
