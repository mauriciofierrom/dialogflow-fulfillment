{-# LANGUAGE OverloadedStrings #-}

module DialogFlow.MessageSpec where

import Data.Aeson (encode)
import Test.Hspec

import DialogFlow.Message

-- TODO: Make tests less flaky. Currently it depends on the order the fields are
-- generated, and iirc the aeson docs states they don't make assurances about
-- it.
spec :: Spec
spec = undefined
