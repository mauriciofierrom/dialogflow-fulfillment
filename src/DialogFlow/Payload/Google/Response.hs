{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module DialogFlow.Payload.Google.Response where

import Data.Aeson

import qualified DialogFlow.Message as M
import DialogFlow.Payload.Google.OtherTypes


data RichMessageType = RMTSimpleResponse
                     | RMTBasicCard
                     | RMTMediaResponse

data Res t where
  SimpleResponse :: M.SimpleResponse -> Res 'RMTSimpleResponse
  BasicCard :: M.Msg 'M.MsgBasicCard -> Res 'RMTBasicCard
  MediaResponse :: MediaType -> [MediaObject] -> Res 'RMTMediaResponse

instance Show (Res t) where
  show = show

instance ToJSON (Res t) where
  toJSON (SimpleResponse s) = toJSON s
  toJSON (BasicCard b) = object [ "basicCard" .= b ]
  toJSON x = toJSON x

data RichResponse where
  RichResponse :: (Show (Res t)) => Res t -> RichResponse

instance ToJSON RichResponse where
  toJSON (RichResponse x) = toJSON x

deriving instance Show RichResponse

data Response =
  Response { expectUserResponse :: Bool
           -- , userStorage :: String
           , richResponse :: [RichResponse] }
           deriving Show

-- TODO: Change RichResponse to Item accordingly
instance ToJSON Response where
  toJSON Response{..} =
    object [ "expectUserResponse" .= expectUserResponse
           -- , "userStorage" .= userStorage
           , "richResponse" .= object [ "items" .= richResponse ] ]
