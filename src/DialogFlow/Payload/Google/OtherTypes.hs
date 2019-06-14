module DialogFlow.Payload.Google.OtherTypes where

data ActionType = UNKNOWN
                | VIEW_DETAILS
                | MODIFY
                | CANCEL
                | RETURN
                | EXCHANGE
                | EMAIl
                | CALL
                | REORDER
                | REVIEW
                | CUSTOMER_SERVICE
                | FIX_ISSUE
                deriving (Show)

data PriceType = UNKNOWN_PRICE
               | ESTIMATE
               | ACTUAL
               deriving (Show)
data Money =
  Money { currencyCode :: String
        , units :: Int
        , nanos :: Int -- int64 format
        } deriving (Show)

data Price = Price PriceType Money deriving Show

data OrderState =
  OrderState { osState :: String
             , osLabel :: String
             } deriving (Show)

data LineItemUpdate =
  LineItemUpdate { liuOrderState :: OrderState
                 , liuPrice :: Price
                 , liuReason :: String
                 , liuExtension :: String -- fields of an arbitrary type
                 } deriving (Show)
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

-- data Item =
--   Item { iTitle :: String
--        , i
