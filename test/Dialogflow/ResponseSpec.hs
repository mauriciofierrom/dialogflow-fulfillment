module Dialogflow.ResponseSpec where

import Test.Hspec

import qualified Data.Map as M

import Dialogflow.Message
import Dialogflow.Response
import TestUtil

responsePath :: FilePath -> FilePath
responsePath = (<>) "files/response/"

spec :: Spec
spec =
  describe "EventInput to/parseJSON instances" $
    it "should have the desired structure" $
      let eventInput = EventInput "the name" (M.fromList [("param1","value1")]) "esES"
       in checkSerialization (responsePath "event_input.json") eventInput
