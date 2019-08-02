module TestUtil where

import Test.Hspec (shouldBe)
import Data.Aeson (encode, decode, ToJSON, FromJSON)
import System.IO (withFile, IOMode(ReadMode))

import qualified Data.ByteString.Lazy as B

checkSerialization :: (FromJSON a, ToJSON a, Show a, Eq a)
                   => FilePath
                   -> a
                   -> IO ()
checkSerialization path x =
  withFile path ReadMode $ \h -> do
    contents <- B.hGetContents h
    Just x `shouldBe` decode contents
