{-# LANGUAGE DeriveGeneric #-}

module Drive.Intercom.Types.Pagination
   where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics


data Pagination = Pagination
  { next  :: Maybe Text
  , prev  :: Maybe Text
  , first :: Maybe Text
  , last  :: Maybe Text
  , page  :: Int
  -- , per_page     :: Int
  -- , total_pages  :: Int
  } deriving (Show, Generic)

instance FromJSON Pagination
