{-# LANGUAGE OverloadedStrings #-}

module DialogFlow.Payload.Google where

import Data.Aeson (object, ToJSON, toJSON, (.=))

import DialogFlow.Payload.Google.Response

newtype GooglePayload =
  GooglePayload { unGooglePayload :: Response } deriving Show

instance ToJSON GooglePayload where
  toJSON gp =
    object [ "google" .= unGooglePayload gp]
