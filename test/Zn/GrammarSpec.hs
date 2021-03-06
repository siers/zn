module Zn.GrammarSpec (spec) where

import Test.Hspec
import Zn.Grammar
import Data.Maybe

unparses match = match `shouldBe` Nothing
parsesAs match expectation = match `shouldBe` Just expectation
parses match string = isJust (matches match string) `shouldBe` True

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

    -- it "is concatenable without end separators" $
    --   matches sed "s/a/b; s/c/d" `parsesAs` [(("a", "b"), ""), (("c", "d"), "")]

  describe "quickfix" $ do
    it "doesn't parse" $
      unparses $ matches quickfix ""

    it "matches" $
      matches quickfix "a → b" `parsesAs` (("a", "b"), "s")

    it "matches with empty" $
      quickfix `parses` " →"

    it "matches with tilde gt" $
      quickfix `parses` " ~>"

    it "matches with recursive star" $
      matches quickfix "a ~>* b" `parsesAs` (("a", "b"), "sg")

  describe "substituteParser" $ do
    it "parses sed" $
      matches sed "s/a/b/ggr" `parsesAs` [(("a", "b"), "ggr")]

    it "parses quickfix" $
      matches substituteParser "a → b" `parsesAs` [(("a", "b"), "s")]

    it "parses quickfix with 's' in front" $
      matches substituteParser "s → s" `parsesAs` [(("s", "s"), "s")]
