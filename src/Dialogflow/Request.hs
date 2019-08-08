{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Dialogflow.Request
Description : Dialogflow types for the webhook request.
Copyright   : (c) Mauricio Fierro, 2019
License     : BSD3-Clause
Maintainer  : Mauricio Fierro <mauriciofierrom@gmail.com>

This module contains types for Dialogflow webhook requests. See the Dialogflow <https://cloud.google.com/dialogflow/docs/reference/rpc/google.cloud.dialogflow.v2#webhookrequest documentation>.
-}

module Dialogflow.Request where

import Data.Aeson ( FromJSON
                  , parseJSON
                  , ToJSON
                  , toJSON
                  , object
                  , withObject
                  , (.:)
                  , (.:!)
                  , (.=))
import Data.List (find)

import qualified Data.Map as M

-- | Represents an intent.
data Intent =
  Intent { intentName :: String  -- ^ Intent name.
         , displayName :: String -- ^ Display name for the intent.
         } deriving (Eq, Show)

instance FromJSON Intent where
  parseJSON = withObject "intent" $ \i -> do
    intentName <- i .: "name"
    displayName <- i .: "displayName"
    return Intent {..}

instance ToJSON Intent where
  toJSON Intent{..} =
    object [ "name" .= intentName
           , "displayName" .= displayName ]

-- | Represents a context.
data Context =
  Context { ctxName :: String
          -- ^ The unique identifier of the context.
          , ctxLifespanCount :: Maybe Int
          -- ^ The number of conversational query requests after which the
          -- context expires.
          , ctxParameters :: M.Map String String
          -- ^ The collection of parameters associated with this context.
          } deriving (Eq, Show)

instance FromJSON Context where
  parseJSON = withObject "context" $ \c -> do
    ctxName <- c .: "name"
    ctxLifespanCount <- c .:! "lifespanCount"
    ctxParameters <- c .: "parameters"
    return Context{..}

instance ToJSON Context where
  toJSON Context{..} =
    object [ "name" .= ctxName
           , "lifespanCount" .= ctxLifespanCount
           , "parameters" .= ctxParameters ]

-- | Represents the result of conversational query or event processing.
data QueryResult =
  QueryResult { queryText :: String
              -- ^ The original text of the query.
              , parameters :: M.Map String String
              -- ^ Consists of parameter_name:parameter_value pairs.
              , allRequiredParamsPresent :: Bool
              -- ^ Set to false if required parameters are missing in query.
              , fulfillmentText :: Maybe String
              -- ^ Text to be pronounced to the user or shown on the screen.
              , outputContexts :: Maybe [Context]
              -- ^ Collection of output contexts.
              , intent :: Maybe Intent
              -- ^ The intent that matched the user's query.
              , intentDetectionConfidence :: Maybe Float
              -- ^ Matching score for the intent.
              , diagnosticInfo :: Maybe (M.Map String String)
              -- ^ Free-form diagnostic info.
              , languageCode :: String
              -- ^ The language that was triggered during intent matching.
              } deriving (Eq, Show)

instance FromJSON QueryResult where
  parseJSON = withObject "queryResult" $ \qr -> do
    queryText <- qr .: "queryText"
    parameters <- qr .: "parameters"
    allRequiredParamsPresent <- qr .: "allRequiredParamsPresent"
    fulfillmentText <- qr .:! "fulfillmentText"
    outputContexts <- qr .:! "outputContexts"
    intent <- qr .:! "intent"
    intentDetectionConfidence <- qr .:! "intentDetectionConfidence"
    diagnosticInfo <- qr .:! "diagnosticInfo"
    languageCode <-qr .: "language_code"
    return QueryResult{..}

instance ToJSON QueryResult where
  toJSON QueryResult{..} =
    object [ "queryText" .= queryText
           , "parameters" .= parameters
           , "allRequiredParamsPresent" .= allRequiredParamsPresent
           , "fulfillmentText" .= fulfillmentText
           , "outputContexts" .= outputContexts
           , "intent" .= intent
           , "diagnosticInfo" .= diagnosticInfo
           , "language_code" .= languageCode ]

-- | The request message for a webhook call.
data WebhookRequest =
  WebhookRequest { responseId :: String
                 -- ^ Unique id for request.
                 , session :: String
                 -- ^ Unique session id.
                 , queryResult :: QueryResult
                 -- ^ Result of the conversation query or event processing.
                 } deriving(Eq, Show)

instance FromJSON WebhookRequest where
  parseJSON = withObject "webhookRequest" $ \wr -> do
    responseId <- wr .: "responseId"
    session <- wr .: "session"
    queryResult <- wr .: "queryResult"
    return WebhookRequest{..}

instance ToJSON WebhookRequest where
  toJSON WebhookRequest{..} =
    object [ "responseId" .= responseId
           , "session" .= session
           , "queryResult" .= queryResult ]

-- TODO: This should be fixed.
-- | Get a context parameter.
getContextParam :: [Context] -- ^ Collection of contexts.
                -> String    -- ^ Name of the context to get the param from.
                -> String    -- ^ Name of the param to get from the context.
                -> Maybe String
getContextParam ctxs ctxName param = do
  mbCtx <- find (ctxByParam param) ctxs
  M.lookup param (ctxParameters mbCtx)
    where
      ctxByParam :: String -> Context -> Bool
      ctxByParam paramName Context{ctxParameters} = M.member paramName ctxParameters
