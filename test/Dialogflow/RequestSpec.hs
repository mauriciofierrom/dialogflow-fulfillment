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
  describe "Context to/parseJSON" $
    it "should have the desired structure" $
      let ctx =
            Context "the context name" (Just 5) (Just $ M.fromList [("param1", "value1"),("param2","value2")])
       in checkSerialization (requestPath "context.json") ctx
  describe "Intent to/parseJSON" $
    it "should have the desired structure" $
      let intent = Intent "the intent name" "the display name"
       in checkSerialization (requestPath "intent.json") intent
  describe "QueryResult" $
    it "should have the desired structure" $
      checkSerialization (requestPath "query_result.json") queryResult
  describe "WebhookRequest to/parseJSON" $
    it "should have the desired structure" $
      let req = WebhookRequest "the response id" "the session" queryResult
       in checkSerialization (requestPath "webhook_request.json") req
    where
      queryResult =
            QueryResult "the original text"
                        (M.fromList [("param1","value1")])
                        True
                        (Just "the fulfillment text")
                        Nothing
                        Nothing
                        (Just 10.5)
                        Nothing
                        "esES"
