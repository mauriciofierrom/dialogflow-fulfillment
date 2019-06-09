module DialogFlow.Util
  ( toObject
  ) where

import Data.Aeson ( ToJSON
                  , toJSON
                  , Value(..)
                  , Object )

toObject :: ToJSON a => a -> Object
toObject a = case toJSON a of
  Object o -> o
  _        -> error "toObject: value isn't an Object"
