module TestUtil where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Aeson (encodeFile, decode, ToJSON, FromJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bool (bool)
import System.Directory ( createDirectoryIfMissing
                        , removeDirectoryRecursive
                        , doesDirectoryExist )
import System.IO (withFile, IOMode(ReadMode))
import Test.Hspec (shouldBe)

import qualified Data.ByteString.Lazy as B

prepareDirs :: FilePath -> IO ()
prepareDirs path = removeRootOutputDir path >> createDirectoryIfMissing True path

removeRootOutputDir :: FilePath -> IO ()
removeRootOutputDir path = do
  exists <- doesDirectoryExist path
  bool (return ()) (removeDirectoryRecursive path) exists

checkSerialization :: (FromJSON a, ToJSON a, Show a, Eq a)
                   => FilePath
                   -> a
                   -> IO ()
checkSerialization path x = do
  B.writeFile path $ encodePretty x
  withFile path ReadMode $ \h -> do
    contents <- B.hGetContents h
    Just x `shouldBe` decode contents
