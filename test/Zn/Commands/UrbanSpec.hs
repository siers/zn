module Zn.Commands.UrbanSpec (spec) where

import Control.Monad.Identity
import Test.Hspec
import Zn.Bot.Request
import Zn.Commands.Urban

-- Obtain your own sample!
-- curl -s http://api.urbandictionary.com/v0/define?term=test

requestMock :: BodyHeaders
requestMock =
    ( "{\"tags\":[],\"result_type\":\"exact\",\"list\":[{\"definition\":\"desc\",\"permalink\":\"\",\"thumbs_up\":442,\"author\":\"tester\",\"word\":\"test\",\"defid\":708924,\"current_vote\":\"\",\"example\":\"This is a test message\",\"thumbs_down\":245}],\"sounds\":[\"http://media.urbandictionary.com/sound/test-8076.mp3\"]}"
    , [])

descriptions = urbanQueryWith (const $ return requestMock) ""

spec :: Spec
spec = do
  describe "full functionality" $ do
    it "works full way" $
       runIdentity (urbanFormat . return <$> descriptions) `shouldBe` "\"test\" :: desc"
