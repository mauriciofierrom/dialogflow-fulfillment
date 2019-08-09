module Dialogflow.ResponseSpec where

import Test.Hspec

import qualified Data.Map as M

import Dialogflow.Response
import TestUtil

basePath :: FilePath
basePath = "files/response/"

responsePath :: FilePath -> FilePath
responsePath = (<>) basePath

spec :: Spec
spec = beforeAll (prepareDirs basePath) $
  describe "EventInput to/parseJSON instances" $
    it "should have the desired structure" $
      let eventInput =
            EventInput "the name"
                       (Just $ M.fromList [("param1","value1")]) "esES"
       in checkSerialization (responsePath "event_input.json") eventInput
