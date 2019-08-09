{-# LANGUAGE OverloadedStrings #-}

module Dialogflow.MessageSpec where

import Test.Hspec
import TestUtil

import Dialogflow.Message

basePath :: FilePath
basePath = "files/message/"

messagePath :: FilePath -> FilePath
messagePath = (<>) basePath

spec :: Spec
spec = beforeAll (prepareDirs basePath) $ do
  describe "SpeechText toJSON" $ do
    context "when it is a TextToSpeech" $
      it "should have the desired structure" $
        let textToSpeech = TextToSpeech "the text"
         in checkSerialization (messagePath "text_to_speech.json") textToSpeech
    context "when it is a SSML" $
      it "should have the desired structure" $
        let ssml = SSML "the xml"
         in checkSerialization (messagePath "ssml.json") ssml
  describe "SimpleResponse toJSON" $ do
    context "when it has a TextToSpeech" $
      it "should have the desired structure" $
        let simpleResponse = SimpleResponse (TextToSpeech "the text") (Just "the maybe text")
         in checkSerialization (messagePath "simple_response_text.json") simpleResponse
    context "when it has a SSML" $
      it "should have the desired structure" $
        let simpleResponse = SimpleResponse (SSML "the xml") (Just "the maybe text")
         in checkSerialization (messagePath "simple_response_ssml.json") simpleResponse
  describe "CardButton toJSON" $
    it "should have the desired structure" $
      let cardButton = CardButton (Just "the text") (Just "the postback")
       in checkSerialization (messagePath "card_button.json") cardButton
  describe "BasicCardContent toJSON" $
    context "when it is an Image" $
      it "should have the desired structure" $
        let image = BasicCardImage (Image "the uri" (Just "the ally text"))
         in checkSerialization (messagePath "basic_card_image.json") image
  describe "OpenUriAction toJSON" $
    it "should have the desired structure" $
      let openUriAction = OpenUriAction "the uri"
       in checkSerialization (messagePath "open_uri_action.json") openUriAction
  describe "BasicCardButton toJSON" $
    it "should have the desired structure" $
      let openUriAction = OpenUriAction "the uri"
          basicCardButton = BasicCardButton "the title" openUriAction
       in checkSerialization (messagePath "basic_card_button.json") basicCardButton
  describe "Suggestion toJSON" $
    it "should have the desired structure" $
      let suggestion = Suggestion "the title"
       in checkSerialization (messagePath "suggestion.json") suggestion
  describe "SelectedItemInfo toJSON" $
    it "should have the desired structure" $
       checkSerialization (messagePath "selected_item_info.json") selectedItemInfo
  describe "Item toJSON" $
    it "should have the desired structure" $
      checkSerialization (messagePath "item.json") item
  describe "Message toJSON" $ do
    context "Text" $
      it "should have the desired structure" $
        let text = Text (Just ["the text"])
         in checkSerialization (messagePath "text.json") text
    context "Image" $
      it "should have the desired structure" $
        let image = Image "the uri" (Just "the ally text")
         in checkSerialization (messagePath "image.json") image
    context "QuickReplies" $
      it "should have the desired structure" $
        let quickReplies = QuickReplies (Just "the title") ["the reply"]
         in checkSerialization (messagePath "quick_replies.json") quickReplies
    context "Card" $
      it "should have the desired structure" $
        let cardButton = CardButton (Just "the text") (Just "the postback")
            card =
              Card (Just "the title") (Just "the subtitle") (Just "the uri") (Just [cardButton])
            in checkSerialization (messagePath "card_image.json") card
    context "SimpleResponses" $
      it "should have the desired structure" $
        let simpleResponses = SimpleResponses [SimpleResponse (TextToSpeech "the text") (Just "the display text")]
           in checkSerialization (messagePath "simple_responses.json") simpleResponses
    context "BasicCard" $ do
      context "when it has an image content" $
        it "should have the desired structure" $
          let image = Image "the uri" (Just "the ally text")
              openUriAction = OpenUriAction "the uri"
              basicCardButton = BasicCardButton "the title" openUriAction
              basicCard =
                BasicCard (Just "the title") (Just "the subtitle") (BasicCardImage image) (Just [basicCardButton])
             in checkSerialization (messagePath "basic_card_with_image.json") basicCard
      context "when it has a formattedText content" $
        it "should have the desired structure" $
          let openUriAction = OpenUriAction "the uri"
              basicCardButton = BasicCardButton "the title" openUriAction
              basicCard =
                BasicCard (Just "the title") (Just "the subtitle") (BasicCardFormattedText "the formatted text") (Just [basicCardButton])
             in checkSerialization (messagePath "basic_card_with_text.json") basicCard
    context "Suggestions" $
      it "should have the desired structure" $
        let suggestions = Suggestions [Suggestion "the suggestion"]
         in checkSerialization (messagePath "suggestions.json") suggestions
    context "LinkOutSuggestion" $
      it "should have the desired structure" $
        let linkOutSuggestion = LinkOutSuggestion "the name" "the app"
         in checkSerialization (messagePath "link_out_suggestion.json") linkOutSuggestion
    context "ListSelect" $
      it "should have the desired structure" $
        let listSelect = ListSelect (Just "the title") [item]
           in checkSerialization (messagePath "list_select.json") listSelect
    context "CarouselSelect" $
      it "should have the desired structure" $
        let carouselSelect = CarouselSelect [item]
           in checkSerialization (messagePath "carousel_select.json") carouselSelect
  where
    selectedItemInfo = SelectItemInfo "the key" (Just ["a synonym"])
    image = Image "the uri" (Just "the ally text")
    item = Item selectedItemInfo "the title" (Just "the description") image
