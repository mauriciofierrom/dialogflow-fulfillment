{-# LANGUAGE OverloadedStrings #-}

module DialogFlow.MessageSpec where

import Data.Aeson (encode)
import Test.Hspec

import DialogFlow.Message

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
      let cardButton = CardButton "the text" "the postback"
          expectedJson = "{\"text\":\"the text\",\"postback\":\"the postback\"}"
       in encode cardButton `shouldBe` expectedJson
