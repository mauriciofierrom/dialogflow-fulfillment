{-# LANGUAGE OverloadedStrings #-}

module DialogFlow.MessageSpec where


import Data.Aeson (encode, decode, ToJSON, FromJSON)
import Test.Hspec
import System.IO (withFile, IOMode(ReadMode))

import qualified Data.ByteString.Lazy as B

import DialogFlow.Message

-- TODO: Make tests less flaky. Currently it depends on the order the fields are
-- generated, and iirc the aeson docs states they don't make assurances about
-- it.
spec :: Spec
spec = do
  describe "SpeechText toJSON" $ do
    context "when it is a TextToSpeech" $
      it "should have the desired structure" $
        let textToSpeech = TextToSpeech "the text"
         in checkSerialization "files/message/text_to_speech.json" textToSpeech
    context "when it is a SSML" $
      it "should have the desired structure" $
        let ssml = SSML "the xml"
         in checkSerialization "files/message/ssml.json" ssml
  describe "SimpleResponse toJSON" $ do
    context "when it has a TextToSpeech" $
      it "should have the desired structure" $
        let simpleResponse = SimpleResponse (TextToSpeech "the text") (Just "the maybe text")
         in checkSerialization "files/message/simple_response_text.json" simpleResponse
    context "when it has a SSML" $
      it "should have the desired structure" $
        let simpleResponse = SimpleResponse (SSML "the xml") (Just "the maybe text")
         in checkSerialization "files/message/simple_response_ssml.json" simpleResponse
  describe "CardButton toJSON" $
    it "should have the desired structure" $
      let cardButton = CardButton (Just "the text") (Just "the postback")
       in checkSerialization "files/message/card_button.json" cardButton
  describe "BasicCardContent toJSON" $
    context "when it is an Image" $
      it "should have the desired structure" $
        let image = BasicCardImage (Image (Just "the uri") (Just "the ally text"))
         in checkSerialization "files/message/basic_card_image.json" image
  describe "OpenUriAction toJSON" $
    it "should have the desired structure" $
      let openUriAction = OpenUriAction "the uri"
       in checkSerialization "files/message/open_uri_action.json" openUriAction
  describe "BasicCardButton toJSON" $
    it "should have the desired structure" $
      let openUriAction = OpenUriAction "the uri"
          basicCardButton = BasicCardButton "the title" openUriAction
       in checkSerialization "files/message/basic_card_button.json" basicCardButton
  describe "Suggestion toJSON" $
    it "should have the desired structure" $
      let suggestion = Suggestion "the title"
       in checkSerialization "files/message/suggestion.json" suggestion
  describe "SelectedItemInfo toJSON" $
    it "should have the desired structure" $
       checkSerialization "files/message/selected_item_info.json" selectedItemInfo
  describe "Item toJSON" $
    it "should have the desired structure" $
      checkSerialization "files/message/item.json" item
  describe "Message toJSON" $ do
    context "Text" $
      it "should have the desired structure" $
        let text = Text ["the text"]
            expectedJson = "{\"text\":[\"the text\"]}"
         in encode text `shouldBe` expectedJson
    context "Image" $
      it "should have the desired structure" $
        let image = Image (Just "the uri") (Just "the ally text")
            expectedJson = "{\"accessibilityText\":\"the ally text\",\"imageUri\":\"the uri\"}"
         in encode image `shouldBe` expectedJson
    context "QuickReplies" $
      it "should have the desired structure" $
        let quickReplies = QuickReplies (Just "the title") ["the reply"]
            expectedJson = "{\"title\":\"the title\",\"quickReplies\":[\"the reply\"]}"
         in encode quickReplies `shouldBe` expectedJson
    context "Card" $
      context "when it has an image content" $
        it "should have the desired structure" $
          let cardButton = CardButton (Just "the text") (Just "the postback")
              card =
                Card (Just "the title") (Just "the subtitle") (Just "the uri") [cardButton]
              expectedJson = "{\"card\":{\"buttons\":[{\"text\":\"the text\",\"postback\":\"the postback\"}]"
                <> ",\"imageUri\":\"the uri\",\"subtitle\":\"the subtitle\""
                <> ",\"title\":\"the title\"}}"
             in encode card `shouldBe` expectedJson
    context "SimpleResponses" $
      it "should have the desired structure" $
        let simpleResponses = SimpleResponses [SimpleResponse (TextToSpeech "the text") (Just "the display text")]
            expectedJson = "{\"simpleResponses\":{\"simpleResponses\":[{\"displayText\":\"the display text\""
              <> ",\"textToSpeech\":\"the text\"}]}}"
           in encode simpleResponses `shouldBe` expectedJson
    context "BasicCard" $ do
      context "when it has an image content" $
        it "should have the desired structure" $
          let image = Image (Just "the uri") (Just "the ally text")
              openUriAction = OpenUriAction "the uri"
              basicCardButton = BasicCardButton "the title" openUriAction
              basicCard =
                BasicCard (Just "the title") (Just "the subtitle") (BasicCardImage image) [basicCardButton]
              imageJson = "{\"accessibilityText\":\"the ally text\",\"imageUri\":\"the uri\"}"
              expectedJson = "{\"image\":" <> imageJson
                <> ",\"buttons\":[{\"openUriAction\":{\"uri\":\"the uri\"},\"title\":\"the title\"}]"
                <> ",\"subtitle\":\"the subtitle\",\"title\":\"the title\"}"
             in encode basicCard `shouldBe` expectedJson
      context "when it has a formattedText content" $
        it "should have the desired structure" $
          let openUriAction = OpenUriAction "the uri"
              basicCardButton = BasicCardButton "the title" openUriAction
              basicCard =
                BasicCard (Just "the title") (Just "the subtitle") (BasicCardFormattedText "the formatted text") [basicCardButton]
              expectedJson = "{\"buttons\":[{\"openUriAction\":{\"uri\":\"the uri\"},\"title\":\"the title\"}]"
                <> ",\"subtitle\":\"the subtitle\",\"title\":\"the title\""
                <> ",\"formattedText\":\"the formatted text\"}"
             in encode basicCard `shouldBe` expectedJson
    context "Suggestions" $
      it "should have the desired structure" $
        let suggestions = Suggestions [Suggestion "the suggestion"]
            expectedJson = "{\"suggestions\":[{\"title\":\"the suggestion\"}]}"
         in encode suggestions `shouldBe` expectedJson
    context "LinkOutSuggestion" $
      it "should have the desired structure" $
        let linkOutSuggestion = LinkOutSuggestion "the name" "the app"
            expectedJson = "{\"uri\":\"the app\",\"destinationName\":\"the name\"}"
         in encode linkOutSuggestion `shouldBe` expectedJson
    context "ListSelect" $
      it "should have the desired structure" $
        let listSelect = ListSelect (Just "the title") [item]
            expectedJson = "{\"items\":[{\"image\":{\"accessibilityText\":\"the ally text\",\"imageUri\":\"the uri\"}"
              <> ",\"title\":\"the title\",\"description\":\"the description\""
              <> ",\"info\":{\"key\":\"the key\",\"synonyms\":[\"a synonym\"]}}]"
              <> ",\"title\":\"the title\"}"
           in encode listSelect `shouldBe` expectedJson
    context "CarouselSelect" $
      it "should have the desired structure" $
        let carouselSelect = CarouselSelect [item]
            expectedJson = "{\"items\":[{\"image\":{\"accessibilityText\":\"the ally text\",\"imageUri\":\"the uri\"}"
              <> ",\"title\":\"the title\",\"description\":\"the description\""
              <> ",\"info\":{\"key\":\"the key\",\"synonyms\":[\"a synonym\"]}}]}"
           in encode carouselSelect `shouldBe` expectedJson
  where
    selectedItemInfo = SelectItemInfo "the key" ["a synonym"]
    image = Image (Just "the uri") (Just "the ally text")
    item = Item selectedItemInfo "the title" "the description" image

checkSerialization :: (FromJSON a, ToJSON a, Show a, Eq a) => FilePath -> a -> IO ()
checkSerialization path x =
  withFile path ReadMode $ \h -> do
    contents <- B.hGetContents h
    Just x `shouldBe` decode contents
