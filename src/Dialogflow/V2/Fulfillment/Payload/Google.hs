{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Module      : Dialogflow.V2.Payload.Google
Description : Dialogflow types for Google Actions payload.
Copyright   : (c) Mauricio Fierro, 2019
License     : BSD3-Clause
Maintainer  : Mauricio Fierro <mauriciofierrom@gmail.com>

This module contains types for the Google Actions payload to be included
in the webhook reponse. See the Dialogflow <https://developers.google.com/actions/build/json/dialogflow-webhook-json#dialogflow-response-body documentation>.
-}

module Dialogflow.V2.Fulfillment.Payload.Google where

import Data.Aeson ( parseJSON
                  , toJSON
                  , withObject
                  , FromJSON
                  , ToJSON
                  , Value(..)
                  , (.=)
                  , (.:)
                  , (.:!) )
import Data.Foldable (asum)
import qualified Data.HashMap.Strict as HM

import Dialogflow.Util
import qualified Dialogflow.V2.Fulfillment.Message as M

-- | This field can be used to provide responses for different platforms
-- like Actions on Google.
newtype GooglePayload =
  GooglePayload { unGooglePayload :: Response } deriving (Eq, Show)

instance ToJSON GooglePayload where
  toJSON gp =
    noNullObjects [ "google" .= unGooglePayload gp]

-- | An image.
data Image =
  Image { iUrl :: String
        , iAccessibilityText :: String
        , iHeight :: Maybe Int
        , iWidth :: Maybe Int
        } deriving (Eq, Show)

instance FromJSON Image where
  parseJSON = withObject "image" $ \i -> do
    iUrl <- i .: "url"
    iAccessibilityText <- i .: "accessibilityText"
    iHeight <- i .:! "height"
    iWidth <- i .:! "width"
    return Image{..}

instance ToJSON Image where
  toJSON Image{..} =
    noNullObjects [ "url" .= iUrl
           , "accessibilityText" .= iAccessibilityText
           , "height" .= iHeight
           , "width" .= iWidth ]

-- | A 'BasicCard' can either contain an image or formatted text.
data BasicCardContent = BasicCardImage Image
                      | BasicCardFormattedText String
                      deriving (Eq, Show)

instance FromJSON BasicCardContent where
  parseJSON = withObject "Image or formatted text" $ \bcc ->
    asum [ BasicCardImage <$> bcc .: "image"
         , BasicCardFormattedText <$> bcc .: "formattedText" ]

instance ToJSON BasicCardContent where
  toJSON = \case
    BasicCardImage image -> noNullObjects [ "image" .= image ]
    BasicCardFormattedText formattedText -> noNullObjects [ "formattedText" .= formattedText ]

-- | Possible image display options for affecting the presentation of an 'Image'.
-- This should be used for when the 'Image''s aspect ratio does not match the 'Image'
-- container's aspect ratio.
data ImageDisplayOption = DEFAULT
                          -- ^ Fill the gaps between the 'Image' and the 'Image' container
                          -- with gray bars.
                        | WHITE
                          -- ^  Fill the gaps between the 'Image' and the 'Image' container
                          -- with white bars.
                        | CROPPED
                          -- ^ 'Image' is scaled such that the 'Image' width and height match
                          -- or exceed the container dimensions. This may crop the top and
                          -- bottom of the 'Image' if the scaled 'Image' height is greater than
                          -- the container height, or crop the left and right of the 'Image'
                          -- if the scaled 'Image' width is greater than the container width.
                          -- This is similar to "Zoom Mode" on a widescreen TV when playing
                          -- a 4:3 video.
                        deriving (Eq, Read, Show)

instance FromJSON ImageDisplayOption where
  parseJSON = withObject "imageDisplayOption" $ \x -> do
    ido <- x .: "imageDisplayOptions"
    return $ read ido

instance ToJSON ImageDisplayOption where
  toJSON x = noNullObjects [ "imageDisplayOptions" .= show x ]

-- | The type of the media within the response.
data MediaType = MEDIA_TYPE_UNSPECIFIED -- ^ Unspecified.
               | AUDIO                  -- ^ Audio stream.
               deriving (Eq, Read, Show)

instance FromJSON MediaType where
  parseJSON = withObject "mediaType" $ \x -> do
    mt <- x .: "mediaType"
    return $ read mt

instance ToJSON MediaType where
  toJSON x = noNullObjects [ "mediaType" .= show x ]

-- | Represents one media noNullObjects which is returned with 'MediaResponse'.
-- Contains information about the media, such as name, description, url, etc.
data MediaObject =
  MediaObject { moName :: String
                -- ^ Name of the 'MediaObject'.
              , moDescription :: String
                -- ^ Description of the 'MediaObject'.
              , moContentUrl :: String
                -- ^ The url pointing to the media content.
              , moLargeImage :: Image
                -- ^ A large 'Image', such as the cover of the album, etc.
              , moIcon :: Image
                -- ^ A small 'Image' icon displayed on the right from the title.
                -- It's resized to 36x36 dp.
              } deriving (Eq, Show)

instance FromJSON MediaObject where
  parseJSON = withObject "MediaObject" $ \mo -> do
    moName <- mo .: "name"
    moDescription <- mo .: "description"
    moContentUrl <- mo .: "contentUrl"
    moLargeImage <- mo .: "largeImage"
    moIcon <- mo .: "icon"
    return MediaObject{..}

instance ToJSON MediaObject where
  toJSON MediaObject{..} =
    noNullObjects [ "name" .= moName
           , "description" .= moDescription
           , "contentUrl" .= moContentUrl
           , "largeImage" .= moLargeImage
           , "icon" .= moIcon ]

-- | The possible types of @RichMessage@s.
data RichMessageType = RMTSimpleResponse
                     | RMTBasicCard
                     | RMTMediaResponse

-- | The response items.
data Res t where
  -- | A simple response containing speech or text to show the user.
  SimpleResponse :: M.SimpleResponse -> Res 'RMTSimpleResponse

  -- | A basic card for displaying some information, e.g. an image and/or text.
  BasicCard :: Maybe String        -- ^ Title.
            -> Maybe String        -- ^ Subtitle.
            -> BasicCardContent    -- ^ Card content can be an image of formatted text.
            -> [M.BasicCardButton] -- ^ Buttons. Currently supports at most 1.
            -> ImageDisplayOption  -- ^ Type of display option.
            -> Res 'RMTBasicCard

  -- | The response indicating a set of media to be played within the conversation.
  MediaResponse :: MediaType     -- ^ Type of the media within this response.
                -> [MediaObject] -- ^ The list of 'MediaObject's.
                -> Res 'RMTMediaResponse

deriving instance Eq (Res t)
deriving instance Show (Res t)

instance FromJSON (Res 'RMTSimpleResponse) where
  parseJSON = withObject "simpleResponse" $ \sr -> do
    simpleResponse <- sr .: "simpleResponse"
    return $ SimpleResponse simpleResponse

instance FromJSON (Res 'RMTBasicCard) where
  parseJSON = withObject "basicCard" $ \basicCard -> do
    bc <- basicCard .: "basicCard"
    mbTitle <- bc .:! "title"
    mbSubtitle <- bc .: "subtitle"
    content <- parseJSON (Object bc)
    buttons <- bc .: "buttons"
    imageDisplayOption <- parseJSON (Object bc)
    return $ BasicCard mbTitle mbSubtitle content buttons imageDisplayOption

instance FromJSON (Res 'RMTMediaResponse) where
  parseJSON = withObject "mediaResponse" $ \mediaResponse -> do
    mr <- mediaResponse .: "mediaResponse"
    mediaType <- parseJSON (Object mr)
    mediaObjects <- mr .: "mediaObjects"
    return $ MediaResponse mediaType mediaObjects

instance ToJSON (Res t) where
  toJSON (SimpleResponse s) = noNullObjects [ "simpleResponse" .=  s ]
  toJSON (BasicCard t s c b d) =
    noNullObjects [ "basicCard" .= obj ]
      where
        obj = Object $ HM.fromList [ "title" .= t
                                   , "subtitle" .= s
                                   , "buttons" .= b ] <> toObject c <> toObject d
  toJSON (MediaResponse mediaType mos) =
    noNullObjects [ "mediaResponse" .= obj ]
      where
        obj = Object $ HM.fromList [ "mediaObjects" .= mos ] <> toObject mediaType

-- | The items to include in the 'RichResponse'
data Item where
  Item :: (Show (Res t)) => Res t -> Item

instance Eq Item where
  (==) (Item x@BasicCard{}) (Item y@BasicCard{}) = x == y
  (==) (Item x@SimpleResponse{}) (Item y@SimpleResponse{}) = x == y
  (==) (Item x@MediaResponse{}) (Item y@MediaResponse{}) = x == y
  (==) _ _ = False

instance ToJSON Item where
  toJSON (Item x) = toJSON x

deriving instance Show Item

-- | A rich response that can include audio, text, cards, suggestions
-- and structured data.
data RichResponse = RichResponse
  { items :: [Item]
  -- ^ A list of UI elements which compose the response. The items must meet
  -- the following requirements:
  -- 1. The first item must be a 'SimpleResponse'
  -- 2. At most two 'SimpleResponse'
  -- 3. At most one rich response item (e.g. 'BasicCard', 'StructuredResponse',
  -- 'MediaResponse', or HtmlResponse)
  -- 4. You cannot use a rich response item if you're using an actions.intent.OPTION
  -- intent ie ListSelect or CarouselSelect
  , suggestions :: [Suggestion]
  -- ^ A list of suggested replies. These will always appear at the end of the response.
  , linkOutSuggestion :: Maybe LinkOutSuggestion
  -- ^  An additional suggestion chip that can link out to the associated app
  -- or site.
  } deriving (Eq, Show)

instance ToJSON RichResponse where
  toJSON RichResponse{..} =
    noNullObjects [ "items" .= items
           , "suggestions" .= suggestions
           , "linkOutSuggestion" .= linkOutSuggestion ]

-- | The response sent by the fulfillment to Google Assistant.
data Response =
  Response { expectUserResponse :: Bool
             -- ^ Indicates whether your fulfillment expects a user response.
             -- Set the value to true when to keep the conversation going and
             -- false to end the conversation.
           , userStorage :: Maybe String
             -- ^ Stores persistent data tied to a specific user. The total storage
             -- amount is 10,000 bytes.
           , richResponse :: RichResponse
             -- ^ This field contains audio, text, cards, suggestions, or structured
             -- data for the Assistant to render. To learn more about using rich
             -- responses for Actions on Google, see 'Res'.
           } deriving (Eq, Show)

instance ToJSON Response where
  toJSON Response{..} =
    noNullObjects [ "expectUserResponse" .= expectUserResponse
           , "userStorage" .= userStorage
           , "richResponse" .= richResponse ]

-- | Different types of url hints.
data UrlTypeHint = URL_TYPE_HINT_UNSPECIFIED
                   -- ^ Unspecified.
                 | AMP_CONTENT
                   -- ^ URL that points directly to AMP content, or to a canonical
                   -- URL which refers to AMP content via .
                 deriving (Eq, Read, Show)

instance FromJSON UrlTypeHint where
  parseJSON = withObject "urlTypeHint" $ \x -> do
    uth <- x .: "urlTypeHint"
    return $ read uth

instance ToJSON UrlTypeHint where
  toJSON x = noNullObjects [ "urlTypeHint" .= show x ]

-- | VersionFilter should be included if specific version/s of the App are
-- required.
data VersionFilter = VersionFilter
  { minVersion :: Int
    -- ^  Min version code or 0, inclusive.
  , maxVersion :: Int
    -- ^ Max version code, inclusive. The range considered is [minVersion:maxVersion].
    -- A null range implies any version.
  } deriving (Eq, Read, Show)

instance FromJSON VersionFilter where
  parseJSON = withObject "versionFilter" $ \vf -> do
    minVersion <- vf .: "minVersion"
    maxVersion <- vf .: "maxVersion"
    return VersionFilter{..}

instance ToJSON VersionFilter where
  toJSON VersionFilter{..} =
    noNullObjects [ "minVersion" .= minVersion
           , "maxVersion" .= maxVersion ]

-- | Specification of the Android App for fulfillment restrictions.
data AndroidApp = AndroidApp
  { aaPackageName :: String
    -- ^ Package name must be specified when specifing Android Fulfillment.
  , aaVersions :: [VersionFilter]
    -- ^ When multiple filters are specified, any filter match will trigger the app.
  } deriving (Eq, Read, Show)

instance FromJSON AndroidApp where
  parseJSON = withObject "androidApp" $ \aa -> do
    aaPackageName <- aa .: "packageName"
    aaVersions <- aa .: "versions"
    return AndroidApp{..}

instance ToJSON AndroidApp where
  toJSON AndroidApp{..} =
    noNullObjects [ "packageName" .= aaPackageName
           , "versions" .= aaVersions ]

-- | Opens the given url.
data OpenUrlAction =
  OpenUrlAction { ouaUrl :: String
                  -- ^ The url field which could be any of: - http/https urls
                  -- for opening an App-linked App or a webpage.
                , ouaAndroidApp :: AndroidApp
                -- ^  Information about the Android App if the URL is expected
                -- to be fulfilled by an Android App.
                , ouaUrlTypeHint :: UrlTypeHint
                -- ^  Indicates a hint for the url type.
                } deriving (Eq, Read, Show)

instance FromJSON OpenUrlAction where
  parseJSON = withObject "openUrlAction" $ \oua -> do
    ouaUrl <- oua .: "url"
    ouaAndroidApp <- oua .: "androidApp"
    ouaUrlTypeHint <- parseJSON (Object oua)
    return OpenUrlAction{..}

instance ToJSON OpenUrlAction where
  toJSON OpenUrlAction{..} =
    Object $ HM.fromList [ "url" .= ouaUrl
                         , "androidApp" .= ouaAndroidApp
                         ] <> toObject ouaUrlTypeHint

-- | Creates a suggestion chip that allows the user to jump out to the App
-- or Website associated with this agent.
data LinkOutSuggestion = LinkOutSuggestion
  { losDestinationName :: String
    -- ^ The name of the app or site this chip is linking to. The chip will be
    -- rendered with the title "Open ". Max 20 chars.
  , losUrl :: String
    -- ^ Deprecated. Use OpenUrlAction instead.
  , losOpenUrlAction :: OpenUrlAction
    -- ^ The URL of the App or Site to open when the user taps the suggestion
    -- chip. Ownership of this App/URL must be validated in the actions on Google
    -- developer console, or the suggestion will not be shown to the user.
  } deriving (Eq, Show)

instance FromJSON LinkOutSuggestion where
  parseJSON = withObject "linkOutSuggestion" $ \los -> do
    losDestinationName <- los .: "destinationName"
    losUrl <- los .: "url"
    losOpenUrlAction <- los .: "openUrlAction"
    return LinkOutSuggestion{..}

instance ToJSON LinkOutSuggestion where
  toJSON LinkOutSuggestion{..} =
    noNullObjects [ "destinationName" .= losDestinationName
           , "url" .= losUrl
           , "openUrlAction" .= losOpenUrlAction ]

-- | A suggestion chip that the user can tap to quickly post a reply to
-- the conversation.
newtype Suggestion = Suggestion { unSuggestion :: String }
  deriving (Eq, Show)

instance FromJSON Suggestion where
  parseJSON = withObject "suggestion" $ \s -> do
    suggestion <- s .: "title"
    return $ Suggestion suggestion

instance ToJSON Suggestion where
  toJSON s = noNullObjects [ "title" .= unSuggestion s ]
