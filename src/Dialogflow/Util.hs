{-|
Module      : Dialogflow.Util
Description : Utilities
Copyright   : (c) Mauricio Fierro, 2019
License     : BSD3-Clause
Maintainer  : Mauricio Fierro <mauriciofierrom@gmail.com>

This module contains misc utility functions.
-}

module Dialogflow.Util
  ( toObject
  , noNullObjects
  ) where

import Data.Aeson ( ToJSON
                  , toJSON
                  , object
                  , Value(..)
                  , Object )
import Data.Aeson.Types (Pair)

-- | Turn a value into an aeson @Object@
toObject :: ToJSON a => a -> Object
toObject a = case toJSON a of
  Object o -> o
  _        -> error "toObject: value isn't an Object"

{-|
   Removes values that were parsed to @Null@ values.
   Useful to avoid having @null@ values in generated
   JSON for @Nothing@ values.
-}
noNullObjects :: [Pair] -> Value
noNullObjects = object . filter (\(_,v) -> v /= Null)
