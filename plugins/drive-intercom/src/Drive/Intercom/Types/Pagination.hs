{-# LANGUAGE DeriveGeneric #-}

module Drive.Intercom.Types.Pagination
   where

import Data.Aeson
import GHC.Generics

import qualified Data.Text          as T


data Pagination = Pagination
  { next        :: Maybe T.Text
  , prev        :: Maybe T.Text
  , first       :: Maybe T.Text
  , last        :: Maybe T.Text
  , page        :: Int
  -- , per_page     :: Int
  -- , total_pages  :: Int
  } deriving (Show, Generic)

instance FromJSON Pagination
