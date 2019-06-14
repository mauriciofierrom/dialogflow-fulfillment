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

instance ToJSON (Res t) where
  toJSON (SimpleResponse s) = toJSON s
  toJSON (BasicCard b) = toJSON b
  toJSON m@(MediaResponse _ _) = toJSON m

data RichResponse where
  RichResponse :: (Show (Res t)) => Res t -> RichResponse

instance ToJSON RichResponse where
  toJSON = toJSON

deriving instance Show RichResponse

data Response =
  Response { expectUserResponse :: Bool
           , userStorage :: String
           , richResponse :: [RichResponse] }
           deriving Show

instance ToJSON Response where
  toJSON Response{..} =
    object [ "expectUserResponse" .= expectUserResponse
           , "userStorage" .= userStorage
           , "richResponse" .= richResponse ]
