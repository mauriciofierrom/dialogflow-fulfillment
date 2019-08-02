{-# LANGUAGE OverloadedStrings #-}

module Dialogflow.MessageSpec where

import Test.Hspec
import TestUtil

import Dialogflow.Message

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
         in checkSerialization "files/message/text.json" text
    context "Image" $
      it "should have the desired structure" $
        let image = Image (Just "the uri") (Just "the ally text")
         in checkSerialization "files/message/image.json" image
    context "QuickReplies" $
      it "should have the desired structure" $
        let quickReplies = QuickReplies (Just "the title") ["the reply"]
         in checkSerialization "files/message/quick_replies.json" quickReplies
    context "Card" $
      it "should have the desired structure" $
        let cardButton = CardButton (Just "the text") (Just "the postback")
            card =
              Card (Just "the title") (Just "the subtitle") (Just "the uri") [cardButton]
            in checkSerialization "files/message/card_image.json" card
    context "SimpleResponses" $
      it "should have the desired structure" $
        let simpleResponses = SimpleResponses [SimpleResponse (TextToSpeech "the text") (Just "the display text")]
            
           in checkSerialization "files/message/simple_responses.json" simpleResponses
    context "BasicCard" $ do
      context "when it has an image content" $
        it "should have the desired structure" $
          let image = Image (Just "the uri") (Just "the ally text")
              openUriAction = OpenUriAction "the uri"
              basicCardButton = BasicCardButton "the title" openUriAction
              basicCard =
                BasicCard (Just "the title") (Just "the subtitle") (BasicCardImage image) [basicCardButton]
             in checkSerialization "files/message/basic_card_with_image.json" basicCard
      context "when it has a formattedText content" $
        it "should have the desired structure" $
          let openUriAction = OpenUriAction "the uri"
              basicCardButton = BasicCardButton "the title" openUriAction
              basicCard =
                BasicCard (Just "the title") (Just "the subtitle") (BasicCardFormattedText "the formatted text") [basicCardButton]
             in checkSerialization "files/message/basic_card_with_text.json" basicCard
    context "Suggestions" $
      it "should have the desired structure" $
        let suggestions = Suggestions [Suggestion "the suggestion"]
         in checkSerialization "files/message/suggestions.json" suggestions
    context "LinkOutSuggestion" $
      it "should have the desired structure" $
        let linkOutSuggestion = LinkOutSuggestion "the name" "the app"
         in checkSerialization "files/message/link_out_suggestion.json" linkOutSuggestion
    context "ListSelect" $
      it "should have the desired structure" $
        let listSelect = ListSelect (Just "the title") [item]
           in checkSerialization "files/message/list_select.json" listSelect
    context "CarouselSelect" $
      it "should have the desired structure" $
        let carouselSelect = CarouselSelect [item]
           in checkSerialization "files/message/carousel_select.json" carouselSelect
  where
    selectedItemInfo = SelectItemInfo "the key" ["a synonym"]
    image = Image (Just "the uri") (Just "the ally text")
    item = Item selectedItemInfo "the title" "the description" image
