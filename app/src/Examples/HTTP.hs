{-# LANGUAGE DeriveGeneric #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy as D
import           Data.Monoid          ((<>))
import qualified Data.Yaml            as Y
import qualified Drive                as D
import qualified Drive.HTTP           as H
import           GHC.Generics         (Generic)


data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  } deriving (Eq, Show, Generic)

instance Y.FromJSON Version


exampleHttpRequest :: D.Free H.HttpF D.ByteString
exampleHttpRequest
  = H.getRaw H.defaults "http://localhost:3000/version.json"


exampleAPIRequest :: D.Free H.HttpF (Maybe Version)
exampleAPIRequest
  = H.get H.defaults "http://localhost:3000/version.json"


main :: IO ()
main = do
  D.foldFree H.execHttp exampleHttpRequest >>= print
  D.foldFree H.execHttp exampleAPIRequest >>= maybe
      (print ("no response" :: String))
      (\v -> print (
        "major: " <> show (major v) <>
        "minor: " <> show (minor v) <>
        "patch: " <> show (patch v)
      ))
