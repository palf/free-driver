{-# LANGUAGE DeriveGeneric #-}

module Main
  ( main
  ) where

import qualified Drive                as D
import qualified GHC.Generics         as G
import qualified Drive.HTTP           as H
import qualified Data.Yaml            as Y
import qualified Data.ByteString.Lazy as D

import Data.Monoid ((<>))

-- import Drive.NetworkSession
-- import Drive.Intercom.Conversation


data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  } deriving (Eq, Show, G.Generic)

instance Y.FromJSON Version


ff :: Monad m => (forall x. f x -> m x) -> D.Free f a -> m a
ff = D.foldFree


header :: String -> IO ()
header t = putStrLn ("\n\n# " <> t)


exampleHttpRequest :: D.Free H.HttpF D.ByteString
exampleHttpRequest
  = H.getRaw H.defaults "http://localhost:3000/version.json"


exampleAPIRequest :: D.Free H.HttpF (Maybe Version)
exampleAPIRequest
  = H.get H.defaults "http://localhost:3000/version.json"


main :: IO ()
main = do
  header "HTTP"
  ff H.execHttp exampleHttpRequest >>= print
  ff H.execHttp exampleAPIRequest >>= \x ->
    case x of
      Nothing -> print ("no response" :: String)
      Just v  -> print (
        "major: " <> show (major v) <>
        "minor: " <> show (minor v) <>
        "patch: " <> show (patch v)
                       )
