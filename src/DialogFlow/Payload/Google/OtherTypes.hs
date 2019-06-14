module DialogFlow.Payload.Google.OtherTypes where

data Image =
  Image { iUrl :: String
        , iAccessibilityText :: String
        , iHeight :: Int
        , iWidth :: Int
        } deriving Show

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
