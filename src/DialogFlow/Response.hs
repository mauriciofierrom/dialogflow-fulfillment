{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module DialogFlow.Response where

import Data.Aeson ( FromJSON
                  , ToJSON
                  , withObject
                  , object
                  , toJSON
                  , parseJSON
                  , (.:)
                  , (.=) )

import GHC.Generics

import DialogFlow.Message

data Response =
  Response { fulfillmentText :: Maybe String
           , fulfillmentMessages :: [FulfillmentMessage]
           , source :: Maybe String
           , payload :: GooglePayload
           } deriving (Eq, Show)

instance FromJSON Response where
  parseJSON = withObject "response" $ \d -> do
    fulfillmentText <- d .: "fulfillmentText"
    fulfillmentMessages <- d .: "fulfillmentMessages"
    source <- d .: "source"
    payload <- d .: "payload"
    return Response{..}

instance ToJSON Response where
  toJSON d = object [
    "fulfillmentText" .= fulfillmentText d,
    "fulfillmentMessages" .= fulfillmentMessages d,
    "source" .= source d,
    "payload" .= payload d ]

newtype FulfillmentMessage = FulfillmentMessage { unSimpleResponses :: SimpleResponses }
  deriving (Eq, Generic, Show)

instance FromJSON FulfillmentMessage
instance ToJSON FulfillmentMessage where
  toJSON s = object [ "simpleResponses" .= unSimpleResponses s ]

data GooglePayload = GooglePayload { expectUserResponse :: Bool
                                   , richResponse :: [SimpleResponse]
                                   } deriving (Eq, Show)

instance FromJSON GooglePayload where
  parseJSON = withObject "payload" $ \gp -> do
    payload <- gp .: "payload"
    googlePayload <- payload .: "google"
    expectUserResponse <- googlePayload .: "expectUserResponse"
    richResponses <- googlePayload .: "richResponse"
    richResponse <- richResponses .: "items"
    return GooglePayload{..}

instance ToJSON GooglePayload where
  toJSON gp =
    object [ "google" .=
      object [ "expectUserResponse" .= expectUserResponse gp
             , "richResponse" .=
               object [ "items" .= richResponse gp ]
             ]
           ]
