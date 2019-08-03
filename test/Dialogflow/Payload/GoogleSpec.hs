module Dialogflow.Payload.GoogleSpec where

import Test.Hspec

import TestUtil
import Dialogflow.Payload.Google

googlePayloadPath :: FilePath -> FilePath
googlePayloadPath = (<>) "files/payload/google/"

spec :: Spec
spec = do
  describe "Image to/parseJSON instances" $
    it "Should have the desired structure" $
      checkSerialization (googlePayloadPath "image.json") image
  describe "BasicCardContent to/parseJSON instances" $ do
    context "when the content is an image" $
      it "should have the desired structure" $
        let basicCardImage = BasicCardImage image
         in checkSerialization (googlePayloadPath "basic_card_image.json") basicCardImage
    context "when the content is formatted text" $
      it "should have the desired structure" $
        let basicCardText = BasicCardFormattedText "the formatted text"
         in checkSerialization (googlePayloadPath "basic_card_formatted_text.json") basicCardText
  describe "ImageDisplayOption to/parseJSON instances" $ do
    context "when the value is DEFAULT" $
      it "should have the desired structure" $
         checkSerialization (googlePayloadPath "image_display_option_default.json") DEFAULT
    context "when the value is WHITE" $
      it "should have the desired structure" $
        checkSerialization (googlePayloadPath "image_display_option_white.json") WHITE
    context "when the value is WHITE" $
      it "should have the desired structure" $
        checkSerialization (googlePayloadPath "image_display_option_cropped.json") CROPPED
  describe "MediaType to/parseJSON instances" $ do
    context "When the value is MEDIA_TYPE_UNSPECIFIED" $
      it "should have the desired structure" $
        checkSerialization (googlePayloadPath "media_type_unspecified.json") MEDIA_TYPE_UNSPECIFIED
    context "When the value is AUDIO" $
      it "should have the desired structure" $
        checkSerialization (googlePayloadPath "media_type_audio.json") AUDIO
  where
    image :: Image
    image = Image "the url" "the ally text" (Just 300) (Just 700)
