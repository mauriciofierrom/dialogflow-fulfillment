{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Dialogflow.Request
Description : Dialogflow requests
Copyright   : (c) Mauricio Fierro, 2019
License     : BSD3-Clause
Maintainer  : Mauricio Fierro <mauriciofierrom@gmail.com>

This module contains types for Dialogflow webhook requests.
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
import GHC.Generics

import qualified Data.Map as M
import Data.List (find)

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
              } deriving (Eq, Generic, Show)

instance FromJSON QueryResult
instance ToJSON QueryResult

-- | The request message for a webhook call.
data WebhookRequest =
  WebhookRequest { responseId :: String
                 -- ^ Unique id for request.
                 , session :: String
                 -- ^ Unique session id.
                 , queryResult :: QueryResult
                 -- ^ Result of the conversation query or event processing.
                 } deriving(Eq, Generic, Show)

instance FromJSON WebhookRequest
instance ToJSON WebhookRequest

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
