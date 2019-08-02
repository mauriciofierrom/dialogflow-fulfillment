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
spec =
  describe "Context FromJSON" $
    it "should decode to a Context type" $
      let ctx =
            Context "the context name" (Just 5) (M.fromList [("param1", "value1"),("param2","value2")])
       in checkSerialization (requestPath "context.json") ctx

