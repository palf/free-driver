{-# LANGUAGE DeriveGeneric #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy as BS
import qualified Drive as V
import qualified Data.Aeson            as Aeson
import           GHC.Generics         (Generic)
import Drive.HTTP
import Drive.Describe
import Drive ((>--->))


data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  } deriving (Eq, Show, Generic)

instance Aeson.FromJSON Version


programRaw :: HttpP BS.ByteString
programRaw
  = getRaw defaults "http://localhost:3000/version.json"


programApi :: HttpP (Maybe Version)
programApi
  = get defaults "http://localhost:3000/version.json"


main :: IO ()
main = do
  describe programRaw >>= print
  run programRaw >>= print

  describe programApi >>= print
  run programApi >>= print

 where
   describe = httpToDescribe >---> execDescribe

   run = V.identityI >---> execHttp
