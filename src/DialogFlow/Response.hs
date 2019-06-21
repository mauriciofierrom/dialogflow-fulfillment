{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DialogFlow.Response where

import Data.Aeson ( object
                  , toJSON
                  , ToJSON
                  , (.=) )


import DialogFlow.Message
import qualified DialogFlow.Payload.Google as G

-- TODO: Output contexts & followup event input
data Response = Response
  { fulfillmentText :: Maybe String -- ^ The text to be shown on the screen
  , fulfillmentMessages :: [Message] -- ^ The collection of rich messages to present to the user
  , source :: Maybe String -- ^ The webhook source
  , payload :: G.GooglePayload -- ^ Webhook payload
  } deriving (Show)

instance ToJSON Response where
  toJSON Response{..} =
    object [ "fulfillmentText" .= fulfillmentText
           , "fulfillmentMessages" .= fulfillmentMessages
           , "source" .= source
           , "payload" .= payload ]
