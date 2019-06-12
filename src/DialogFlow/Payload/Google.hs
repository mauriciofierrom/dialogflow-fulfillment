{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module DialogFlow.Payload.Google
  (
  ) where

import qualified DialogFlow.Message as M
import DialogFlow.OtherTypes

data RichMessageType = RMTSimpleResponse
                     | RMTBasicCard
                     | RMTStructuredResponse
                     | RMTMediaResponse
                     | RMTCarouselBrowse
                     | TableCard


data Action =
  Action { aType :: ActionType
         , aButton :: M.BasicCardButton
         } deriving (Show)

data UserNotification =
  UserNotification { unTitle :: String
                   , unText :: String
                   } deriving Show

newtype Info = Info { unInfo :: String } deriving Show

data OrderUpdate =
  OrderUpdate { googleOrderId :: String
              , actionOrderId :: String
              , orderState :: OrderState
              , orderManagementActions :: [Action]
              , updateTime :: String -- Should be a timestamp
              , totalPrice :: Price
              , lineItemUpdates :: LineItemUpdate
              , userNotification :: UserNotification
              , infoExtension :: String -- some type object
              , info :: Info
              } deriving Show

data Res t where
  -- SimpleResponse :: SimpleResponse -> Res 'RMTSimpleResponse
  -- BasicCard :: BasicCard -> Res 'RMTBasicCard
  StructuredResponse :: OrderUpdate -> Res 'RMTStructuredResponse
  MediaResponse :: MediaType -> [MediaObject] -> Res 'RMTMediaResponse

data RichResponse where
  RichResponse :: (Show (Res t)) => Res t -> RichResponse

data Response =
  Response { expectUserResponse :: Bool
           , userStoreage :: String
           , richResponse :: [RichResponse] }
