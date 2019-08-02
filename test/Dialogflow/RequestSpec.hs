{-# LANGUAGE OverloadedStrings #-}

module Dialogflow.RequestSpec where

import Data.Aeson (eitherDecode)
import Test.Hspec
import qualified Data.Map as M

import Dialogflow.Request
import TestUtil

requestPath :: FilePath -> FilePath
requestPath = (<>) "files/request/"

spec :: Spec
spec = do
  describe "Context FromJSON" $
    it "should have the desired structure" $
      let ctx =
            Context "the context name" (Just 5) (M.fromList [("param1", "value1"),("param2","value2")])
       in checkSerialization (requestPath "context.json") ctx
  describe "Intent FromJSON" $
    it "should have the desired structure" $
      let intent = Intent "the intent name" "the display name"
       in checkSerialization (requestPath "intent.json") intent

