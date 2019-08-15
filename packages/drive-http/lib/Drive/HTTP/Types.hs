{-# LANGUAGE DeriveFunctor #-}

module Drive.HTTP.Types
  ( HttpF (..)
  , HttpUriF (..)
  , HttpHeaderF (..)
  , HttpError (..)
  ) where

import qualified Data.ByteString.Lazy as BS
import qualified Network.Wreq         as Wreq


type Uri = String


data HttpError
  = RequestError
  | NoContent
  deriving (Show)


data HttpF a
  = Get Wreq.Options Uri (BS.ByteString -> a)
  deriving (Functor)


data HttpUriF x a
  = GetUri Wreq.Options (x -> Uri) (Either HttpError BS.ByteString -> a)
  deriving (Functor)


data HttpHeaderF y a
  = GetOptions (y -> Wreq.Options) Uri (BS.ByteString -> a)
  deriving (Functor)
