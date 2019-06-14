{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module DialogFlow.Payload.Google.Response where

import qualified DialogFlow.Message as M
import DialogFlow.Payload.Google.OtherTypes

data RichMessageType = RMTSimpleResponse
                     | RMTBasicCard
                     | RMTMediaResponse

data Res t where
  SimpleResponse :: M.SimpleResponse -> Res 'RMTSimpleResponse
  BasicCard :: M.Msg 'M.MsgBasicCard -> Res 'RMTBasicCard
  MediaResponse :: MediaType -> [MediaObject] -> Res 'RMTMediaResponse

data RichResponse where
  RichResponse :: (Show (Res t)) => Res t -> RichResponse

deriving instance Show RichResponse

data Response =
  Response { expectUserResponse :: Bool
           , userStoreage :: String
           , richResponse :: [RichResponse] }
           deriving Show
