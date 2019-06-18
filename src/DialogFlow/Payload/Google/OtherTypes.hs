{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DialogFlow.Payload.Google.OtherTypes where

import Data.Aeson

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

data MediaObject =
  MediaObject { moName :: String
              , moDescription :: String
              , moContentUrl :: String
              , moLargeImage :: Image
              , moIcon :: Image
              } deriving Show
