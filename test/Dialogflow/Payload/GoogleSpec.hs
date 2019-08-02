module Dialogflow.Payload.GoogleSpec where

import Test.Hspec

import TestUtil
import Dialogflow.Payload.Google

googlePayloadPath :: FilePath -> FilePath
googlePayloadPath = (<>) "files/payload/google/"

spec :: Spec
spec =
  describe "Image to/parseJSON instances" $ do
    it "Should have the desired structure" $
      let image = Image "the url" "the ally text" (Just 300) (Just 700)
       in checkSerialization (googlePayloadPath "image.json") image
