{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Module      : Dialogflow.Util
Description : Dialogflow types for response messages.
Copyright   : (c) Mauricio Fierro, 2019
License     : BSD3-Clause
Maintainer  : Mauricio Fierro <mauriciofierrom@gmail.com>

This module contains types for Dialogflow messages to be used in
a fulfillment webhook response. see the Dialogflow <https://cloud.
google.com/dialogflow/docs/reference/rpc/google.cloud.dialogflow.v2
#google.cloud.dialogflow.v2.Intent.Message documentation>.
-}

module Dialogflow.Message
  ( CardButton(..)
  , BasicCardContent(..)
  , BasicCardButton(..)
  , Item(..)
  , OpenUriAction(..)
  , SpeechText(..)
  , SimpleResponse(..)
  , Suggestion(..)
  , SelectItemInfo(..)
  , MsgType(..)
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
                  , (.:!)
                  , (.=))
import Data.Foldable (asum)

import qualified Data.HashMap.Strict as HM

import Dialogflow.Util

-- | Button for a 'Card' message
data CardButton = CardButton
  { cbText     :: Maybe String -- ^ The text to show on the button.
  , cbPostback :: Maybe String -- ^ The text to send to the Dialogflow API or URI to open.
  } deriving (Eq, Show)

instance FromJSON CardButton where
  parseJSON = withObject "cardButton" $ \cb -> do
    cbText <- cb .:! "text"
    cbPostback <- cb .:! "postback"
    return CardButton{..}

instance ToJSON CardButton where
  toJSON CardButton{..} =
    object [ "text" .= cbText
           , "postback" .= cbPostback ]

-- | The 'BasicCard' message can have either an 'Image' or
-- formatted text as content.
data BasicCardContent = BasicCardImage (Msg 'MsgImage)
                      | BasicCardFormattedText String
                      deriving (Eq, Show)

instance FromJSON BasicCardContent where
  parseJSON = withObject "Image or formatted text" $ \bcc ->
    asum [ BasicCardImage <$> bcc .: "image"
         , BasicCardFormattedText <$> bcc .: "formatted_text" ]

instance ToJSON BasicCardContent where
  toJSON = \case
    BasicCardImage image -> object [ "image" .= image ]
    BasicCardFormattedText formattedText -> object [ "formatted_text" .= formattedText ]

-- | A 'SimpleResponse' can have text-to-speech in plain text
-- or SSML format.
data SpeechText = TextToSpeech String -- ^ The plain text of the speech output
                | SSML String         -- ^ Structured spoken response to the user in SSML format
                deriving (Eq, Show)

instance FromJSON SpeechText where
  parseJSON = withObject "textToSpeech or SSML" $ \st ->
    asum [ TextToSpeech <$> st .: "textToSpeech"
         , SSML <$> st .: "ssml" ]

instance ToJSON SpeechText where
  toJSON = \case
    TextToSpeech textToSpeech -> object ["textToSpeech" .= textToSpeech]
    SSML ssml -> object ["ssml" .= ssml]

-- | A simple response message containing speech or text
data SimpleResponse = SimpleResponse
  { simpleResponseText :: SpeechText   -- ^ The speech text
  , displayText        :: Maybe String -- ^ The text to display
  } deriving (Eq, Show)

instance FromJSON SimpleResponse where
  parseJSON = withObject "simpleResponse" $ \sr -> do
    simpleResponseText <- parseJSON (Object sr)
    displayText <- sr .:! "displayText"
    return SimpleResponse{..}

instance ToJSON SimpleResponse where
  toJSON SimpleResponse{..} = Object $
    toObject simpleResponseText <> HM.fromList ["displayText" .= displayText ]

-- | An action to open the given URI. Used in 'BasicCardButton's.
newtype OpenUriAction = OpenUriAction
  { unOpenUriAction :: String -- ^ The HTTP or HTTPS scheme URI
  } deriving (Eq, Show)

instance ToJSON OpenUriAction where
  toJSON oua = object [ "uri" .= unOpenUriAction oua ]

instance FromJSON OpenUriAction where
  parseJSON = withObject "openUriAction" $ \oua -> do
    uri <- oua .: "uri"
    return $ OpenUriAction uri

-- | Buttons for 'BasicCard's.
data BasicCardButton = BasicCardButton
  { bcbTitle :: String                -- ^ The title of the button
  , bcbOpenUriAction :: OpenUriAction -- ^ Action to take when a user taps on the button
  } deriving (Eq, Show)

instance FromJSON BasicCardButton where
  parseJSON = withObject "basicCardButton" $ \bcb -> do
    bcbTitle <- bcb .: "title"
    bcbOpenUriAction <- bcb .: "openUriAction"
    return BasicCardButton{..}

instance ToJSON BasicCardButton where
  toJSON BasicCardButton{..} =
    object [ "title" .= bcbTitle
           , "openUriAction" .= bcbOpenUriAction ]

-- | Suggestion chips.
newtype Suggestion = Suggestion
  { unSuggestionTitle :: String -- ^ The text shown in the suggestion chip
  } deriving (Eq, Show)

instance FromJSON Suggestion where
  parseJSON = withObject "suggestion" $ \s -> do
    unSuggestionTitle <- s .: "title"
    return $ Suggestion unSuggestionTitle

instance ToJSON Suggestion where
  toJSON s =
    object [ "title" .= unSuggestionTitle s ]

-- | Additional information about an 'Item' for when it is triggered in a dialog.
data SelectItemInfo = SelectItemInfo
  { siiKey      :: String
  -- ^ A unique key that will be sent back to the agent if this response is given.
  , siiSynonyms :: [String]
  -- ^ A list of synonyms that can also be used to trigger this item in dialog.
  } deriving (Eq, Show)

instance FromJSON SelectItemInfo where
  parseJSON = withObject "selectedItemInfo" $ \sii -> do
    siiKey <- sii .: "key"
    siiSynonyms <- sii .: "synonyms"
    return SelectItemInfo{..}

instance ToJSON SelectItemInfo where
  toJSON SelectItemInfo{..} =
    object [ "key" .= siiKey
           , "synonyms" .= siiSynonyms ]

-- | The possible types of 'Message'.
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

-- | The messages to be included in the Response.
data Msg t where
  -- | The text response message.
  Text
    :: Maybe [String] -- ^ The collection of the agent's responses
    -> Msg 'MsgText

  -- | The image response message.
  Image
    :: String -- ^ The public URI to an image file
    -> Maybe String -- ^ A text description of the image to be used for accessibility
    -> Msg 'MsgImage

  -- | The quick replies response message.
  QuickReplies
    :: Maybe String   -- ^ The title of the collection of quick replies
    -> [String]       -- ^ The collection of quick replies
    -> Msg 'MsgQuickReplies

  -- | The card response.
  Card
    :: Maybe String -- ^ The title of the card
    -> Maybe String -- ^ The subtitle of the card
    -> Maybe String -- ^ The public URI to an image file for the card
    -> Maybe [CardButton] -- ^ The collection of card buttons
    -> Msg 'MsgCard

  -- | The collection of 'SimpleResponse' candidates.
  SimpleResponses
    :: [SimpleResponse] -- ^ The list of simple responses
    -> Msg 'MsgSimpleResponses

-- TODO: Check if the formattedText and image fields are mutually exclusive
  -- | The basic card message. Useful for displaying information.
  BasicCard
    :: Maybe String      -- ^ The title of the card
    -> Maybe String      -- ^ The subtitle of the card
    -> BasicCardContent  -- ^ The body text or image of the card
    -> Maybe [BasicCardButton] -- ^ The collection of card buttons
    -> Msg 'MsgBasicCard

  -- | The collection of 'Suggestion'.
  Suggestions
    :: [Suggestion] -- ^ The list of suggested replies
    -> Msg 'MsgSuggestions

  -- | The suggestion chip message that allows the user to jump
  -- out to the app or the website associated with this agent.
  LinkOutSuggestion
    :: String -- ^ The name of the app or site this chip is linking to
    -> String -- ^ The URI of the app or site to open when the user taps the suggestion chip
    -> Msg 'MsgLinkOutSuggestion

  -- | The card for presenting a list of options to select from.
  ListSelect
    :: Maybe String -- ^ The overall title of the list
    -> [Item] -- ^ List items
    -> Msg 'MsgListSelect

  -- | The card for representing a carousel of options to select from.
  CarouselSelect
    :: [Item] -- ^ Carousel items
    -> Msg 'MsgCarouselSelect

deriving instance Show (Msg t)
deriving instance Eq (Msg t)

instance FromJSON (Msg 'MsgImage) where
  parseJSON = withObject "image" $ \i -> do
    uri <- i .: "image_uri"
    allyText <- i .:! "accessibility_text"
    return (Image uri allyText)

instance FromJSON (Msg 'MsgText) where
  parseJSON = withObject "text" $ \t -> do
    text <- t .: "text"
    return $ Text text

instance FromJSON (Msg 'MsgQuickReplies) where
  parseJSON = withObject "quickReplies" $ \qr -> do
    title <- qr .:! "title"
    replies <- qr .: "quick_replies"
    return $ QuickReplies title replies

instance FromJSON (Msg 'MsgCard) where
  parseJSON = withObject "card" $ \card -> do
    c <- card .: "card"
    mbTitle <- c .:! "title"
    mbSubtitle <- c .:! "subtitle"
    mbUri <- c .:! "image_uri"
    cardButtons <- c .:! "buttons"
    return $ Card mbTitle mbSubtitle mbUri cardButtons

instance FromJSON (Msg 'MsgSimpleResponses) where
  parseJSON = withObject "simpleResponses" $ \sr -> do
    srs <- sr .: "simpleResponses"
    responses <- srs .: "simpleResponses"
    return $ SimpleResponses responses

instance FromJSON (Msg 'MsgBasicCard) where
  parseJSON = withObject "basicCard" $ \bc -> do
    mbTitle <- bc .:! "title"
    mbSubtitle <- bc .:! "subtitle"
    content <- parseJSON (Object bc)
    buttons <- bc .:! "buttons"
    return $ BasicCard mbTitle mbSubtitle content buttons

instance FromJSON (Msg 'MsgSuggestions) where
  parseJSON = withObject "suggestions" $ \sgs -> do
    suggestions <- sgs .: "suggestions"
    return $ Suggestions suggestions

instance FromJSON (Msg 'MsgLinkOutSuggestion) where
  parseJSON = withObject "linkOutSuggestion" $ \los -> do
    uri <- los .: "uri"
    destinationName <- los .: "destination_name"
    return $ LinkOutSuggestion destinationName uri

instance FromJSON (Msg 'MsgListSelect) where
  parseJSON = withObject "listSelect" $ \ls -> do
    title <- ls .:! "title"
    items <- ls .: "items"
    return $ ListSelect title items

instance FromJSON (Msg 'MsgCarouselSelect) where
  parseJSON = withObject "carouselSelect" $ \cs -> do
    items <- cs .: "items"
    return $ CarouselSelect items

instance ToJSON (Msg t) where
  toJSON (Text mbText) = object [ "text" .= mbText ]
  toJSON (Image uri accesibilityText) =
    object [ "image_uri" .= uri
           , "accessibility_text" .= accesibilityText ]
  toJSON (QuickReplies mbTitle quickReplies) =
    object [ "title" .= mbTitle
           , "quick_replies" .= quickReplies ]
  toJSON (Card title subtitle imageUri buttons) =
    object [ "card" .= object ["title" .= title
                              , "subtitle" .= subtitle
                              , "image_uri" .= imageUri
                              , "buttons" .= buttons ] ]
  toJSON (SimpleResponses simpleResponses) =
    object [ "simpleResponses" .= object ["simpleResponses" .= simpleResponses ] ]
  toJSON (BasicCard mbTitle mbSubtitle content buttons) =
    Object $ HM.fromList [ "title" .= mbTitle
                         , "subtitle" .= mbSubtitle
                         , "buttons" .= buttons ] <> toObject content
  toJSON (Suggestions xs) = object [ "suggestions" .= xs ]
  toJSON (LinkOutSuggestion name uri) =
    object [ "destination_name" .= name, "uri" .= uri ]
  toJSON (ListSelect mbTitle items) =
    object [ "title" .= mbTitle
           , "items" .= items ]
  toJSON (CarouselSelect items) = object [ "items" .= items ]

-- | This type is used to wrap the messages under one type.
data Message where
  Message :: (Show (Msg t)) => Msg t -> Message

instance Show Message where
  show (Message o) = show o

instance FromJSON Message where
  parseJSON = parseJSON

instance ToJSON Message where
  toJSON (Message bc@BasicCard{}) = object [ "basicCard" .= toJSON bc ]
  toJSON (Message o) = toJSON o

instance Eq Message where
  (==) (Message x@Text{}) (Message y@Text{}) = x == y
  (==) (Message x@Image{}) (Message y@Image{}) = x == y
  (==) (Message x@QuickReplies{}) (Message y@QuickReplies{}) = x == y
  (==) (Message x@Card{}) (Message y@Card{}) = x == y
  (==) (Message x@SimpleResponses{}) (Message y@SimpleResponses{}) = x == y
  (==) (Message x@BasicCard{}) (Message y@BasicCard{}) = x == y
  (==) (Message x@Suggestions{}) (Message y@Suggestions{}) = x == y
  (==) (Message x@LinkOutSuggestion{}) (Message y@LinkOutSuggestion{}) = x == y
  (==) (Message x@ListSelect{}) (Message y@ListSelect{}) = x == y
  (==) (Message x@CarouselSelect{}) (Message y@CarouselSelect{}) = x == y
  (==) _ _ = False

-- | An item in 'ListSelect' and 'CarouselSelect'.
data Item = Item
  { iInfo :: SelectItemInfo -- ^ Additional information about this option
  , iTitle :: String -- ^ The title of the list item
  , iDescription :: String -- ^ The main text describing the item
  , iImage :: Msg 'MsgImage -- ^ The image to display
  } deriving (Eq, Show)

instance FromJSON Item where
    parseJSON = withObject "Item" $ \i -> do
      iInfo <- i .: "info"
      iTitle <- i .: "title"
      iDescription <- i .: "description"
      iImage <- i .: "image"
      return Item{..}

instance ToJSON Item where
  toJSON Item{..} =
    object [ "info" .= iInfo
           , "title" .= iTitle
           , "description" .= iDescription
           , "image" .= iImage ]
