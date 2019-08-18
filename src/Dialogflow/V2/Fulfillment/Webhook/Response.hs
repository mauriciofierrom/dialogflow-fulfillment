{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Dialogflow.Util
Description : Dialogflow types for the webhook response.
Copyright   : (c) Mauricio Fierro, 2019
License     : BSD3-Clause
Maintainer  : Mauricio Fierro <mauriciofierrom@gmail.com>

This module contains types for Dialogflow webhook response. See the Dialogflow <https://cloud.google.com/dialogflow/docs/reference/rpc/google.cloud.dialogflow.v2#webhookresponse documentation>.
-}

module Dialogflow.V2.Fulfillment.Webhook.Response where

import Data.Aeson ( parseJSON
                  , toJSON
                  , withObject
                  , FromJSON
                  , ToJSON
                  , (.:)
                  , (.=) )
import Dialogflow.Util (noNullObjects)

import qualified Data.Map as M

import Dialogflow.V2.Fulfillment.Webhook.Request (Context)
import Dialogflow.V2.Fulfillment.Message

import qualified Dialogflow.V2.Fulfillment.Payload.Google as G

-- TODO: When this is included, no messages or payload is taken into account.
-- We gotta cover this.
-- | Makes the platform immediately invoke another DetectIntent call internally
-- with the specified event as input. When this field is set, Dialogflow ignores
-- the fulfillment_text, fulfillment_messages, and payload fields.
data EventInput =
  EventInput { eventInputName :: String
             , eventInputParameters :: Maybe (M.Map String String)
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
    noNullObjects [ "name" .= eventInputName
           , "parameters" .= eventInputParameters
           , "language_code" .= eventInputLanguageCode ]

-- | The response message for a webhook call.
data WebhookResponse = WebhookResponse
  { fulfillmentText :: Maybe String
  -- ^ The text to be shown on the screen
  , fulfillmentMessages :: Maybe [Message]
  -- ^ The collection of rich messages to present to the user
  , source :: Maybe String
  -- ^ The webhook source
  , payload :: Maybe G.GooglePayload
  -- ^ Webhook payload
  , outputContexts :: Maybe [Context]
  -- ^ The collection of output contexts
  , followupEventInput :: Maybe EventInput
  -- ^ Makes the platform immediately invoke another sessions
  } deriving (Eq, Show)

instance ToJSON WebhookResponse where
  toJSON WebhookResponse{..} =
    noNullObjects [ "fulfillmentText" .= fulfillmentText
           , "fulfillmentMessages" .= fulfillmentMessages
           , "source" .= source
           , "payload" .= payload
           , "outputContexts" .= outputContexts
           , "followupEventInput" .= followupEventInput ]
