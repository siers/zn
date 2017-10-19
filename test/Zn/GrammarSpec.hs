module Zn.GrammarSpec (spec) where

import Test.Hspec
import Zn.Grammar

unparses match = match `shouldBe` Nothing
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

  describe "sed" $ do
    it "doesn't parse" $
      unparses $ matches sed ""

    it "parses" $
      matches sed "s/a/b/ggr" `parsesAs` [(("a", "b"), "ggr")]

    it "parses without separator" $
      matches sed "s/a/b" `parsesAs` [(("a", "b"), "")]

    it "is concatenable" $
      matches sed "s/a/b/; s/c/d" `parsesAs` [(("a", "b"), ""), (("c", "d"), "")]

    it "is concatenable without end separators" $
      matches sed "s/a/b; s/c/d" `parsesAs` [(("a", "b"), ""), (("c", "d"), "")]
