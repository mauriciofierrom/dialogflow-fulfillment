module DialogFlow.Util
  ( toObject
  , noNullObjects
  ) where

import Data.Aeson ( ToJSON
                  , toJSON
                  , object
                  , Value(..)
                  , Object )
import Data.Aeson.Types (Pair)

toObject :: ToJSON a => a -> Object
toObject a = case toJSON a of
  Object o -> o
  _        -> error "toObject: value isn't an Object"

noNullObjects :: [Pair] -> Value
noNullObjects = object . filter (\(_,v) -> v /= Null)
