{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module DialogFlow.Request where

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

data Intent =
  Intent { intentName :: String -- ^ Intent name
         , displayName :: String -- ^ Display name for the intent
         } deriving (Eq, Generic, Show)

instance FromJSON Intent where
  parseJSON = withObject "intent" $ \i -> do
    intentName <- i .: "name"
    displayName <- i .: "displayName"
    return Intent {..}

data Context =
  Context { ctxName :: String
          -- ^ The unique identifier of the context
          , ctxLifespanCount :: Maybe Int
          -- ^ The number of conversational query requests after which the
          -- context expires
          , ctxParameters :: M.Map String String
          -- ^ The collection of parameters associated with this context
          } deriving (Eq, Generic, Show)

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

-- TODO: Include Messages when FromJSON instances are added
data QueryResult =
  QueryResult { queryText :: String
              -- ^ The original text of the query
              , parameters :: M.Map String String
              -- ^ Consists of parameter_name:parameter_value pairs
              , allRequiredParamsPresent :: Bool
              -- ^ Set to false if required parameters are missing in query
              , fulfillmentText :: Maybe String
              -- ^ Text to be pronounced to the user or shown on the screen
              , outputContexts :: Maybe [Context]
              -- ^ Collection of output contexts
              , intent :: Maybe Intent
              -- ^ The intent that matched the user's query
              , intentDetectionConfidence :: Maybe Float
              -- ^ Matching score for the intent
              , diagnosticInfo :: Maybe (M.Map String String)
              -- ^ Free-form diagnostic info
              , languageCode :: String
              -- ^ The language that was triggered during intent matching
              } deriving (Generic, Show)

instance FromJSON QueryResult

data Request =
  Request { responseId :: String
          -- ^ Unique id for request
          , session :: String
          -- ^ Unique session id
          , queryResult :: QueryResult
          -- ^ Result of the conversation query or event processing
          } deriving(Generic, Show)

instance FromJSON Request
