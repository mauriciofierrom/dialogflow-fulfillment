{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DialogFlow.Message
  ( CardButton(..)
  , BasicCardContent(..)
  , ListItem(..)
  , CarouselItem(..)
  , SpeechText(..)
  , SimpleResponse(..)
  , Msg( Text
       , Image
       , QuickReplies
       , Card
       , SimpleResponses
       , BasicCard
       , Suggestions
       , LinkOutSuggestion
       , ListSelect
       , CarouselSelect
       )
  , Message(..)
  ) where

import Data.Aeson ( FromJSON
                  , parseJSON
                  , ToJSON
                  , toJSON
                  , object
                  , Value(..)
                  , withObject
                  , (.:)
                  , (.=))
import Data.Foldable (asum)

import qualified Data.HashMap.Strict as HM

import DialogFlow.Util

data CardButton = CardButton
  { cbText :: String -- ^ The text to show on the button
  , cbPostback :: String -- ^ The text to send to the DialogFlow API or URI to open
  } deriving (Eq, Show)

instance ToJSON CardButton where
  toJSON CardButton{..} =
    object [ "text" .= cbText
           , "postback" .= cbPostback ]

data BasicCardContent = BasicCardImage (Msg 'MsgImage)
                      | BasicCardFormattedText String
                      deriving (Show)

instance ToJSON BasicCardContent where
  toJSON = \case
    BasicCardImage image -> object [ "image" .= image ]
    BasicCardFormattedText formattedText -> object [ "formattedText" .= formattedText ]


data SpeechText = TextToSpeech String -- ^ The plain text of the speech output
                | SSML String -- ^ Structured spoken response to the user in SSML format
                deriving (Eq, Show)

instance FromJSON SpeechText where
  parseJSON = withObject "textToSpeech or SSML" $ \st ->
    asum [ TextToSpeech <$> st .: "textToSpeech"
         , SSML <$> st .: "ssml" ]

instance ToJSON SpeechText where
  toJSON = \case
    TextToSpeech textToSpeech -> object ["textToSpeech" .= textToSpeech]
    SSML ssml -> object ["ssml" .= ssml]

data SimpleResponse = SimpleResponse
  { simpleResponseText :: SpeechText -- ^ The speech text
  , displayText :: Maybe String -- ^ The text to display
  } deriving (Eq, Show)

instance ToJSON SimpleResponse where
  toJSON SimpleResponse{..} = Object $
    toObject simpleResponseText <> HM.fromList ["displayText" .= displayText ]

newtype OpenUriAction = OpenUriAction
  { unOpenUriAction :: String -- ^ The HTTP or HTTPS scheme URI
  } deriving (Eq, Show)

data BasicCardButton = BasicCardButton
  { bcbTitle :: String -- ^ The title of the button
  , bcbOpenUriAction :: OpenUriAction -- ^ Action to take when a user taps on the button
  } deriving (Eq, Show)

newtype Suggestion = Suggestion
  { unSuggestionTitle :: String -- ^ The text shown in the suggestion chip
  } deriving (Eq, Show)

data SelectItemInfo = SelectItemInfo
  { siiKey :: String -- ^ A unique key that will be sent back to the agent if this response is given
  , siiSynonyms :: Maybe [String] -- ^ A list of synonyms that can also be used to trigger this item in dialog
  } deriving (Eq, Show)


data MsgType = MsgText
             | MsgImage
             | MsgQuickReplies
             | MsgCard
             | MsgSimpleResponses
             | MsgBasicCard
             | MsgSuggestions
             | MsgLinkOutSuggestion
             | MsgListSelect
             | MsgCarouselSelect

data Msg t where
  Text
    :: [String] -- ^ The collection of the agent's responses
    -> Msg 'MsgText

  Image
    :: Maybe String -- ^ The public URI to an image file
    -> Maybe String -- ^ A text description of the image to be used for accessibility
    -> Msg 'MsgImage

  QuickReplies
    :: Maybe String   -- ^ The title of the collection of quick replies
    -> Maybe [String] -- ^ The collection of quick replies
    -> Msg 'MsgQuickReplies

  Card
    :: Maybe String -- ^ The title of the card
    -> Maybe String -- ^ The subtitle of the card
    -> Maybe String -- ^ The public URI to an image file for the card
    -> Maybe [CardButton] -- ^ The collection of card buttons
    -> Msg 'MsgCard

  SimpleResponses
    :: [SimpleResponse] -- ^ The list of simple responses
    -> Msg 'MsgSimpleResponses

-- TODO: Check if the formattedText and image fields are mutually exclusive
  BasicCard
    :: Maybe String -- ^ The title of the card
    -> Maybe String -- ^ The subtitle of the card
    -> BasicCardContent -- ^ The body text or image of the card
    -> Maybe [BasicCardButton] -- ^ The collection of card buttons
    -> Msg 'MsgBasicCard

  Suggestions
    :: [Suggestion] -- ^ The list of suggested replies
    -> Msg 'MsgSuggestions

  LinkOutSuggestion
    :: String -- ^ The name of the app or site this chip is linking to
    -> String -- ^ The URI of the app or site to open when the user taps the suggestion chip
    -> Msg 'MsgLinkOutSuggestion

  ListSelect
    :: Maybe String -- ^ The overall title of the list
    -> [ListItem] -- ^ List items
    -> Msg 'MsgListSelect

  CarouselSelect
    :: [CarouselItem] -- ^ Carousel items
    -> Msg 'MsgCarouselSelect

data Message where
  Message :: (Show (Msg t)) => Msg t -> Message

-- TODO: Make sure homoiconicity is respected
instance Show (Msg t) where
  show (Text ts) = "Text " <> show ts
  show (Image mbUri mbAccessibilityText) =
    "Image " <> show mbUri <> " " <> show mbAccessibilityText
  show (QuickReplies  mbTitle mbReplies) =
    "QuickReplies " <> show mbTitle <> " " <> show mbReplies
  show (Card mbTitle mbSubtitle mbUri mbCardButtons) =
    "Card " <> show mbTitle
            <> " " <> show mbSubtitle
            <> " " <> show mbUri
            <> " " <> show mbCardButtons
  show (SimpleResponses simpleResponses) = "SimpleRespnses " <> show simpleResponses
  show (BasicCard mbTitle mbSubtitle mbBasicCardContent mbBasicCardButtons) =
    "BasicCard " <> show mbTitle
                 <> " " <> show mbSubtitle
                 <> " " <> show mbBasicCardContent
                 <> " " <> show mbBasicCardButtons
  show (Suggestions suggestions) = "Suggestions " <> show suggestions
  show (LinkOutSuggestion name uri) = "LinkOuSuggestion " <> name <> " " <> uri
  show (ListSelect mbTitle items) = "ListSelect " <> show mbTitle <> " " <> show items
  show (CarouselSelect items) = "CarouselSelect " <> show items

instance ToJSON (Msg t) where
  toJSON (Image uri accesibilityText) =
    object [ "imageUri" .= uri
           , "accessibilityText" .= accesibilityText ]

data ListItem = ListItem
  { liInfo :: SelectItemInfo -- ^ Additional information about this option
  , liTitle :: String -- ^ The title of the list item
  , liDescription :: String -- ^ The main text describing the item
  , liImage :: Msg 'MsgImage -- ^ The image to display
  } deriving (Show)

data CarouselItem = CarouselItem
  { ciInfo :: SelectItemInfo -- ^ Additional info about the option item
  , ciTitle :: String -- ^ Title of the carousel item
  , ciDescription :: String -- ^ The body text of the card
  , ciImage :: Msg 'MsgImage -- ^ The image to display
  } deriving (Show)
