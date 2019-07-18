{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module DialogFlow.Payload.Google where

import DialogFlow.Util

import qualified DialogFlow.Message as M
import qualified Data.HashMap.Strict as HM

import Data.Aeson (object, Value(..), ToJSON, toJSON, (.=))

newtype GooglePayload =
  GooglePayload { unGooglePayload :: Response } deriving Show

instance ToJSON GooglePayload where
  toJSON gp =
    object [ "google" .= unGooglePayload gp]

data Image =
  Image { iUrl :: String
        , iAccessibilityText :: String
        , iHeight :: Maybe Int
        , iWidth :: Maybe Int
        } deriving Show

instance ToJSON Image where
  toJSON Image{..} =
    object [ "url" .= iUrl
           , "accessibilityText" .= iAccessibilityText
           , "height" .= iHeight
           , "width" .= iWidth ]

data BasicCardContent = BasicCardImage Image
                      | BasicCardFormattedText String
                      deriving (Show)

instance ToJSON BasicCardContent where
  toJSON = \case
    BasicCardImage image -> object [ "image" .= image ]
    BasicCardFormattedText formattedText -> object [ "formattedText" .= formattedText ]

data ImageDisplayOption = DEFAULT
                         | WHITE
                         | CROPPED
                         deriving Show

instance ToJSON ImageDisplayOption where
  toJSON x = object [ "imageDisplayOptions" .= show x ]

data MediaType = MEDIA_TYPE_UNSPECIFIED
               | AUDIO
               deriving Show

instance ToJSON MediaType where
  toJSON x = object [ "mediaType" .= show x ]

data MediaObject =
  MediaObject { moName :: String
              , moDescription :: String
              , moContentUrl :: String
              , moLargeImage :: Image
              , moIcon :: Image
              } deriving Show

instance ToJSON MediaObject where
  toJSON MediaObject{..} =
    object [ "name" .= moName
           , "description" .= moDescription
           , "contentUrl" .= moContentUrl
           , "largeImage" .= moLargeImage
           , "icon" .= moIcon ]

data RichMessageType = RMTSimpleResponse
                     | RMTBasicCard
                     | RMTMediaResponse

data Res t where
  SimpleResponse :: M.SimpleResponse -> Res 'RMTSimpleResponse
  BasicCard :: Maybe String -- ^ Title
            -> Maybe String -- ^ Subtitle
            -> BasicCardContent -- ^ Card content can be an image of formatted text
            -> [M.BasicCardButton] -- ^ Buttons. Currently supports at most 1.
            -> ImageDisplayOption -- ^ Type of display option
            -> Res 'RMTBasicCard
  MediaResponse :: MediaType -> [MediaObject] -> Res 'RMTMediaResponse

instance Show (Res t) where
  show = show

instance ToJSON (Res t) where
  toJSON (SimpleResponse s) = object [ "simpleResponse" .=  s ]
  toJSON (BasicCard t s c b d) =
    object [ "basicCard" .= obj ]
      where
        obj = Object $ HM.fromList [ "title" .= t
                                   , "subtitle" .= s
                                   , "buttons" .= b ] <> toObject c <> toObject d
  toJSON (MediaResponse mediaType mos) =
    object [ "mediaResponse" .= (toObject mediaType <> toObject mos) ]

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
