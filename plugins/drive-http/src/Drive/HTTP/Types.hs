{-# LANGUAGE DeriveFunctor #-}

module Drive.HTTP.Types
  ( HttpF (..)
  , HttpUriF (..)
  , HttpHeaderF (..)
  , HttpError (..)
  ) where

import qualified Data.ByteString.Lazy as D
import qualified Network.Wreq         as W


type Uri = String


data HttpError
  = RequestError
  | NoContent
  deriving (Show)


data HttpF a
  = Get W.Options Uri (D.ByteString -> a)
  deriving (Functor)


data HttpUriF x a
  = GetUri W.Options (x -> Uri) (Either HttpError D.ByteString -> a)
  deriving (Functor)


data HttpHeaderF y a
  = GetOptions (y -> W.Options) Uri (D.ByteString -> a)
  deriving (Functor)
