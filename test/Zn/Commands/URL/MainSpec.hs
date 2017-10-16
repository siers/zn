module Zn.Commands.URL.MainSpec (spec) where

import Test.Hspec
import Zn.Commands.URL.Main

spec :: Spec
spec = do
  describe "title fetching" $ do
    it "doesn't match empty" $
       parseTitle "" `shouldBe` Nothing

    it "ignores case" $
       parseTitle "<tiTle>foo" `shouldBe` Just "foo"
