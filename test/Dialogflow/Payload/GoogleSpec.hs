module Dialogflow.Payload.GoogleSpec where

import Test.Hspec

import TestUtil
import Dialogflow.Payload.Google
import Data.Aeson

import qualified Dialogflow.Message as M

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
  describe "MediaObject to/parseJSON instances" $
    it "should have the desired structure" $
      let mediaObject = MediaObject "the name" "the description" "the content url" image image
       in checkSerialization (googlePayloadPath "media_object.json") mediaObject
  describe "UrlTypeHint to/parseJSON instances" $ do
    context "URL_TYPE_HINT_UNSPECIFIED" $
      it "should have the desired structure" $
        checkSerialization (googlePayloadPath "url_type_hint_unspecified.json") URL_TYPE_HINT_UNSPECIFIED
    context "AMP_CONTENT" $
      it "should have the desired structure" $
        checkSerialization (googlePayloadPath "url_type_hint_amp_content.json") AMP_CONTENT
  describe "VersionFilter to/parseJSON instances" $
    it "should have the desired structure" $
      let versionFilter = VersionFilter 1 2
       in checkSerialization (googlePayloadPath "version_filter.json") versionFilter
  describe "AndroidApp to/parseJSON instances" $
    it "should have the desired structure" $
      checkSerialization (googlePayloadPath "android_app.json") androidApp
  describe "OpenUrlAction to/parseJSON instances" $
    it "should have the desired structure" $
      let openUrlAction = OpenUrlAction "the url" androidApp URL_TYPE_HINT_UNSPECIFIED
       in checkSerialization (googlePayloadPath "open_url_action.json") openUrlAction
  where
    image :: Image
    image = Image "the url" "the ally text" (Just 300) (Just 700)

    androidApp :: AndroidApp
    androidApp = AndroidApp "the package name" [VersionFilter 1 2]