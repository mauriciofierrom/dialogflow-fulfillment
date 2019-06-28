{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DialogFlow.Response where

import Data.Aeson ( object
                  , toJSON
                  , ToJSON
                  , (.=) )

import qualified Data.Map as M

import DialogFlow.Request (Context)
import DialogFlow.Message

import qualified DialogFlow.Payload.Google as G

data EventInput =
  EventInput { eventInputName :: String
             , eventInputParameters :: M.Map String String
             , eventInputLanguageCode :: String
             } deriving Show

instance ToJSON EventInput where
  toJSON EventInput{..} =
    object [ "name" .= eventInputName
           , "parameters" .= eventInputParameters
           , "languageCode" .= eventInputLanguageCode ]

-- TODO: Output contexts & followup event input
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

instance ToJSON Response where
  toJSON Response{..} =
    object [ "fulfillmentText" .= fulfillmentText
           , "fulfillmentMessages" .= fulfillmentMessages
           , "source" .= source
           , "payload" .= payload
           , "outputContexts" .= outputContexts
           , "followupEventInput" .= followupEventInput ]
