{-# LANGUAGE OverloadedStrings #-}

module DialogFlow.MessageSpec where

import Data.Aeson (encode)
import Test.Hspec

import DialogFlow.Message

-- TODO: Make tests less flaky. Currently it depends on the order the fields are
-- generated, and iirc the aeson docs states they don't make assurances about
-- it.
spec :: Spec
spec = do
  describe "SpeechText toJSON" $ do
    context "when it is a TextToSpeech" $ do
      it "should have the desired structure" $
        let textToSpeech = TextToSpeech "the text"
         in encode textToSpeech `shouldBe` "{\"textToSpeech\":\"the text\"}"
    context "when it is a SSML" $ do
      it "shourld have the desired structure" $
        let ssml = SSML "the xml"
         in encode ssml `shouldBe` "{\"ssml\":\"the xml\"}"
  describe "SimpleResponse toJSON" $ do
    context "when it has a TextToSpeech" $
      it "should have the desired structure" $
        let expectedJson =
              "{\"displayText\":\"the maybe text\",\"textToSpeech\":\"the text\"}"
            simpleResponse = SimpleResponse (TextToSpeech "the text") (Just "the maybe text")
         in encode simpleResponse `shouldBe` expectedJson
    context "when it has a SSML" $
      it "should have the desired structure" $
        let expectedJson =
              "{\"displayText\":\"the maybe text\",\"ssml\":\"the xml\"}"
            simpleResponse = SimpleResponse (SSML "the xml") (Just "the maybe text")
         in encode simpleResponse `shouldBe` expectedJson
  describe "CardButton toJSON" $ do
    it "should have the desired structure" $
      let cardButton = CardButton (Just "the text") (Just "the postback")
          expectedJson = "{\"text\":\"the text\",\"postback\":\"the postback\"}"
       in encode cardButton `shouldBe` expectedJson
  describe "BasicCardContent toJSON" $ do
    context "when it is an Image" $
      it "should have the desired structure" $
        let image = BasicCardImage (Image (Just "the uri") (Just "the ally text"))
            expectedJson = "{\"image\":{\"accessibilityText\":\"the ally text\",\"imageUri\":\"the uri\"}}"
         in encode image `shouldBe` expectedJson
  describe "OpenUriAction toJSON" $
    it "should have the desired structure" $
      let openUriAction = OpenUriAction "the uri"
          expectedJson = "{\"uri\":\"the uri\"}"
       in encode openUriAction `shouldBe` expectedJson
  describe "BasicCardButton toJSON" $ do
    it "should have the desired structure" $
      let openUriAction = OpenUriAction "the uri"
          basicCardButton = BasicCardButton "the title" openUriAction
          expectedJson = "{\"openUriAction\":{\"uri\":\"the uri\"},\"title\":\"the title\"}"
       in encode basicCardButton `shouldBe` expectedJson
  describe "Suggestion toJSON" $ do
    it "should have the desired structure" $
      let suggestion = Suggestion "the title"
          expectedJson = "{\"title\":\"the title\"}"
       in encode suggestion `shouldBe` expectedJson
  describe "SelectedItemInfo toJSON" $ do
    it "should have the desired structure" $
      let selectedItemInfo = SelectItemInfo "the key" ["a synonym"]
          expectedJson = "{\"key\":\"the key\",\"synonyms\":[\"a synonym\"]}"
       in encode selectedItemInfo `shouldBe` expectedJson
  describe "Item toJSON" $ do
    it "should have the desired structure" $
      let selectedItemInfo = SelectItemInfo "the key" ["a synonym"]
          image = Image (Just "the uri") (Just "the ally text")
          item = Item selectedItemInfo "the title" "the description" image
          expectedJson = "{\"image\":{\"accessibilityText\":\"the ally text\",\"imageUri\":\"the uri\"}"
            <> ",\"title\":\"the title\",\"description\":\"the description\""
            <> ",\"info\":{\"key\":\"the key\",\"synonyms\":[\"a synonym\"]}}"
       in encode item `shouldBe` expectedJson
  describe "Message toJSON" $ do
    context "Text" $ do
      it "should have the desired structure" $
        let text = Text ["the text"]
            expectedJson = "{\"text\":[\"the text\"]}"
         in encode text `shouldBe` expectedJson
    context "Image" $ do
      it "should have the desired structure" $
        let image = Image (Just "the uri") (Just "the ally text")
            expectedJson = "{\"accessibilityText\":\"the ally text\",\"imageUri\":\"the uri\"}"
         in encode image `shouldBe` expectedJson
    context "QuickReplies" $
      it "should have the desired structure" $
        let quickReplies = QuickReplies (Just "the title") ["the reply"]
            expectedJson = "{\"title\":\"the title\",\"quickReplies\":[\"the reply\"]}"
         in encode quickReplies `shouldBe` expectedJson
    context "Card" $ do
      context "when it has an image content" $
        it "should have the desired structure" $
          let cardButton = CardButton (Just "the text") (Just "the postback")
              card =
                Card (Just "the title") (Just "the subtitle") (Just "the uri") [cardButton]
              expectedJson = "{\"buttons\":[{\"text\":\"the text\",\"postback\":\"the postback\"}]"
                <> ",\"imageUri\":\"the uri\",\"subtitle\":\"the subtitle\""
                <> ",\"title\":\"the title\"}"
             in encode card `shouldBe` expectedJson
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
    context "Suggestions" $ do
      it "should have the desired structure" $
        let suggestions = Suggestions [Suggestion "the suggestion"]
            expectedJson = "{\"suggestions\":[{\"title\":\"the suggestion\"}]}"
         in encode suggestions `shouldBe` expectedJson
    context "LinkOutSuggestion" $ do
      it "should have the desired structure" $
        let linkOutSuggestion = LinkOutSuggestion "the name" "the app"
            expectedJson = "{\"uri\":\"the app\",\"destinationName\":\"the name\"}"
         in encode linkOutSuggestion `shouldBe` expectedJson
    context "ListSelect" $ do
      it "should have the desired structure" $
        let selectedItemInfo = SelectItemInfo "the key" ["a synonym"]
            image = Image (Just "the uri") (Just "the ally text")
            item = Item selectedItemInfo "the title" "the description" image
            listSelect = ListSelect (Just "the title") [item]
            expectedJson = "{\"items\":[{\"image\":{\"accessibilityText\":\"the ally text\",\"imageUri\":\"the uri\"}"
              <> ",\"title\":\"the title\",\"description\":\"the description\""
              <> ",\"info\":{\"key\":\"the key\",\"synonyms\":[\"a synonym\"]}}]"
              <> ",\"title\":\"the title\"}"
           in encode listSelect `shouldBe` expectedJson
    context "CarouselSelect" $ do
      it "should have the desired structure" $
        let selectedItemInfo = SelectItemInfo "the key" ["a synonym"]
            image = Image (Just "the uri") (Just "the ally text")
            item = Item selectedItemInfo "the title" "the description" image
            carouselSelect = CarouselSelect [item]
            expectedJson = "{\"items\":[{\"image\":{\"accessibilityText\":\"the ally text\",\"imageUri\":\"the uri\"}"
              <> ",\"title\":\"the title\",\"description\":\"the description\""
              <> ",\"info\":{\"key\":\"the key\",\"synonyms\":[\"a synonym\"]}}]}"
           in encode carouselSelect `shouldBe` expectedJson
