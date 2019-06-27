{-# LANGUAGE OverloadedStrings #-}

module DialogFlow.RequestSpec where

import Data.Aeson (eitherDecode)
import Test.Hspec

import qualified Data.Map as M

import DialogFlow.Request

spec :: Spec
spec = do
  describe "Context FromJSON" $
    it "should decode to a Context type" $
      let json = "{\"name\":\"the name\",\"lifespanCount\":5,\"parameters\":{\"param\":\"param value\"}}"
          ctx = Context "the name" (Just 5) (M.fromList [("param", "param value")])
          Right s = eitherDecode json :: Either String Context
       in s `shouldBe` ctx

