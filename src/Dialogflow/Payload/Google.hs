{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Dialogflow.Payload.Google where

import Dialogflow.Util

import qualified Dialogflow.Message as M
import qualified Data.HashMap.Strict as HM

import Data.Aeson (object, parseJSON, Value(..), FromJSON, withObject, ToJSON, toJSON, (.=), (.:))

-- | This field can be used to provide responses for different platforms
-- like Actions on Google.
newtype GooglePayload =
  GooglePayload { unGooglePayload :: Response } deriving Show

instance ToJSON GooglePayload where
  toJSON gp =
    object [ "google" .= unGooglePayload gp]

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
    iHeight <- i .: "height"
    iWidth <- i .: "width"
    return Image{..}

instance ToJSON Image where
  toJSON Image{..} =
    object [ "url" .= iUrl
           , "accessibilityText" .= iAccessibilityText
           , "height" .= iHeight
           , "width" .= iWidth ]


-- | A 'BasicCard' can either contain an image or formatted text.
data BasicCardContent = BasicCardImage Image
                      | BasicCardFormattedText String
                      deriving (Show)

instance ToJSON BasicCardContent where
  toJSON = \case
    BasicCardImage image -> object [ "image" .= image ]
    BasicCardFormattedText formattedText -> object [ "formattedText" .= formattedText ]

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
                        deriving Show

instance ToJSON ImageDisplayOption where
  toJSON x = object [ "imageDisplayOptions" .= show x ]

-- | The type of the media within the response.
data MediaType = MEDIA_TYPE_UNSPECIFIED -- ^ Unspecified.
               | AUDIO                  -- ^ Audio stream.
               deriving Show

instance ToJSON MediaType where
  toJSON x = object [ "mediaType" .= show x ]

-- | Represents one media object which is returned with 'MediaResponse'.
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

data Item where
  Item :: (Show (Res t)) => Res t -> Item

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
  } deriving Show

instance ToJSON RichResponse where
  toJSON RichResponse{..} =
    object [ "items" .= items
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
           } deriving Show

instance ToJSON Response where
  toJSON Response{..} =
    object [ "expectUserResponse" .= expectUserResponse
           , "userStorage" .= userStorage
           , "richResponse" .= richResponse ]

-- | Different types of url hints.
data UrlTypeHint = URL_TYPE_HINT_UNSPECIFIED
                   -- ^ Unspecified.
                 | AMP_CONTENT
                   -- ^ URL that points directly to AMP content, or to a canonical
                   -- URL which refers to AMP content via .
                 deriving (Eq, Show)

instance ToJSON UrlTypeHint where
  toJSON x = object [ "urlTypeHint" .= show x ]

-- | VersionFilter should be included if specific version/s of the App are
-- required.
data VersionFilter = VersionFilter
  { minVersion :: Int
    -- ^  Min version code or 0, inclusive.
  , maxVersion :: Int
    -- ^ Max version code, inclusive. The range considered is [minVersion:maxVersion].
    -- A null range implies any version.
  } deriving (Eq, Show)

instance ToJSON VersionFilter where
  toJSON VersionFilter{..} =
    object [ "minVersion" .= minVersion
           , "maxVersion" .= maxVersion ]

-- | Specification of the Android App for fulfillment restrictions.
data AndroidApp = AndroidApp
  { aaPackageName :: String
    -- ^ Package name must be specified when specifing Android Fulfillment.
  , aaVersions :: [VersionFilter]
    -- ^ When multiple filters are specified, any filter match will trigger the app.
  } deriving (Eq, Show)

instance ToJSON AndroidApp where
  toJSON AndroidApp{..} =
    object [ "packageName" .= aaPackageName
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
                } deriving (Eq, Show)

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

instance ToJSON LinkOutSuggestion where
  toJSON LinkOutSuggestion{..} =
    object [ "destinationName" .= losDestinationName
           , "url" .= losUrl
           , "openUrlAction" .= losOpenUrlAction ]

-- | A suggestion chip that the user can tap to quickly post a reply to
-- the conversation.
newtype Suggestion = Suggestion { unSuggestion :: String }
  deriving (Eq, Show)

instance ToJSON Suggestion where
  toJSON s = object [ "title" .= unSuggestion s ]
