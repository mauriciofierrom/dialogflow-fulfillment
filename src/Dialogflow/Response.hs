{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Dialogflow.Response where

import Data.Aeson ( object
                  , parseJSON
                  , toJSON
                  , withObject
                  , FromJSON
                  , ToJSON
                  , (.:)
                  , (.=) )

import qualified Data.Map as M

import Dialogflow.Request (Context)
import Dialogflow.Message

import qualified Dialogflow.Payload.Google as G

data EventInput =
  EventInput { eventInputName :: String
             , eventInputParameters :: M.Map String String
             , eventInputLanguageCode :: String
             } deriving (Eq, Show)

instance FromJSON EventInput where
  parseJSON = withObject "eventInput" $ \ei -> do
    eventInputName <- ei .: "name"
    eventInputParameters <- ei .: "parameters"
    eventInputLanguageCode <- ei .: "language_code"
    return EventInput{..}

instance ToJSON EventInput where
  toJSON EventInput{..} =
    object [ "name" .= eventInputName
           , "parameters" .= eventInputParameters
           , "language_code" .= eventInputLanguageCode ]

data Response = Response
  { fulfillmentText :: Maybe String
  -- ^ The text to be shown on the screen
  , fulfillmentMessages :: [Message]
  -- ^ The collection of rich messages to present to the user
  , source :: Maybe String
  -- ^ The webhook source
  , payload :: G.GooglePayload
  -- ^ Webhook payload
  , outputContexts :: Maybe [Context]
  -- ^ The collection of output contexts
  , followupEventInput :: Maybe EventInput
  -- ^ Makes the platform immediately invoke another sessions
  } deriving (Show)

instance FromJSON Response where
  parseJSON = withObject "response" $ \r -> do
    fulfillmentText <- r .: "fulfillmentText"
    fulfillmentMessages <- r .: "fulfillmentMessages"
    source <- r .: "source"
    payload <- r .: "payload"
    outputContexts <- r .: "outputContexts"
    followupEventInput <- r .: "followupEventInput"
    return Response{..}

instance ToJSON Response where
  toJSON Response{..} =
    object [ "fulfillmentText" .= fulfillmentText
           , "fulfillmentMessages" .= fulfillmentMessages
           , "source" .= source
           , "payload" .= payload
           , "outputContexts" .= outputContexts
           , "followupEventInput" .= followupEventInput ]
