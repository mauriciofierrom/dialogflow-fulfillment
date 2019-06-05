{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module DialogFlow.Message
  ( Button(..)
  , Text(..)
  , SimpleResponses(..)
  , SimpleResponse(..)
  , SpeechText(..)
  ) where

import Data.Aeson ( FromJSON
                  , parseJSON
                  , ToJSON
                  , toJSON
                  , object
                  , Value(..)
                  , Object
                  , withObject
                  , (.:)
                  , (.=))
import Data.Foldable (asum)
import GHC.Generics

import qualified Data.HashMap.Strict as HM

newtype Text = Text { text :: Maybe [String] } deriving (Eq, Show)

instance FromJSON Text where
  parseJSON = withObject "text" $ \o -> do
    firstText <- o .: "text"
    text <- firstText .: "text"
    return Text{..}

instance ToJSON Text where
  toJSON t = object [
    "text" .= text t ]

newtype SimpleResponses = SimpleResponses { simpleResponses :: [SimpleResponse] }
  deriving (Eq, Generic, Show)

instance ToJSON SimpleResponses where
  toJSON msr = object [ "simpleResponses" .= simpleResponses msr]

instance FromJSON SimpleResponses where
  parseJSON = withObject "simpleResponses" $ \msr ->
    SimpleResponses <$> msr .: "simpleResponses"


data Image = Image { iImageUri :: Maybe String
                     , accessibilityText :: Maybe String
                     } deriving (Eq, Generic, Show)

data QuickReply = QuickReply { title :: Maybe String
                             , quickReplies :: Maybe [String]
                             } deriving (Eq, Generic, Show)

data Button = Button { buttonText :: Maybe String
                     , buttonPostback :: Maybe String
                     } deriving (Eq, Generic, Show)

data Card = Card { cCardTitle :: Maybe String
                 , cSubtitle :: Maybe String
                 , cImageUri :: Maybe String
                 , cButtons :: Maybe [Button]
                 } deriving (Eq, Generic, Show)

data SpeechText = TextToSpeech String | SSML String deriving (Eq, Show)

instance FromJSON SpeechText where
  parseJSON = withObject "textToSpeech or SSML" $ \st ->
    asum [ TextToSpeech <$> st .: "textToSpeech"
         , SSML <$> st .: "ssml" ]

instance ToJSON SpeechText where
  toJSON = \case
    TextToSpeech textToSpeech -> object ["textToSpeech" .= textToSpeech]
    SSML ssml -> object ["ssml" .= ssml]

data SimpleResponse =
  SimpleResponse { simpleResponseText :: SpeechText
                 , displayText :: Maybe String
                 } deriving (Eq, Show)

instance FromJSON SimpleResponse where
  parseJSON = withObject "simpleResponse" $ \sr ->
    SimpleResponse <$> sr .: "textToSpeech" <*> sr .: "displayText"

instance ToJSON SimpleResponse where
  toJSON SimpleResponse{..} = Object $
    toObject simpleResponseText <> HM.fromList ["displayText" .= displayText ]

data BasicCard =
  BasicCard { bsTitle :: Maybe String
            , bsSubtitle :: Maybe String
            , bsFormattedText :: String
            , bsImage :: Maybe Image
            , bsButtons :: [Button]
            } deriving (Eq, Generic, Show)

newtype Suggestions =
  Suggestions { sugTitle :: String } deriving (Eq, Generic, Show)

data LinkOutSuggestion =
  LinkOutSuggestion { losDestinationName :: String
                    , losUri :: String
                    } deriving (Eq, Generic, Show)

data SelectItemInfo =
  SelectItemInfo { siiKey :: String
                 , siiSynonyms :: Maybe [String] } deriving (Eq, Generic, Show)

data Item =
  Item { iInfo :: SelectItemInfo
       , iTitle :: String
       , iDescription :: Maybe String
       , iImage :: Maybe Image
       } deriving (Eq, Generic, Show)

data ListSelect =
  ListSelect { lsTitle :: Maybe String
             , lsItems :: [Item]
             } deriving (Eq, Generic, Show)

newtype CarouselSelect =
  CarouselSelect { csItems :: [Item] } deriving (Eq, Generic, Show)

toObject :: ToJSON a => a -> Object
toObject a = case toJSON a of
  Object o -> o
  _        -> error "toObject: value isn't an Object"
