{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DialogFlow.Payload.Google where

import Data.Aeson

import DialogFlow.Payload.Google.Response

newtype GooglePayload =
  GooglePayload { unGooglePayload :: Response } deriving Show

instance ToJSON GooglePayload where
  toJSON GooglePayload{..} =
    object [ "google" .= unGooglePayload ]
