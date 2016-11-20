{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Drive.Intercom.Types.Admins
  ( Admin (..)
  , AdminID (..)
  , AdminsResponse (..)
  , nullAdmin
  , adminIDToString
  ) where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

import qualified Drive.Intercom.Types.Pagination as P

adminIDToString :: AdminID -> String
adminIDToString (AdminID t) = T.unpack t

newtype AdminID = AdminID T.Text
  deriving (Show, Generic, FromJSON)

data AdminsResponse = AdminsResponse
  { aPages :: Maybe P.Pagination
  , admins :: [Admin]
  } deriving (Show, Generic)

instance FromJSON AdminsResponse where
  parseJSON = withObject "adminResponse" $ \v -> do
    aPages' <- v .: "pages"
    admins' <- v .: "admins"

    pure AdminsResponse
      { aPages = aPages'
      , admins = admins'
      }


data Admin = Admin
  { adminID    :: AdminID
  , adminName  :: T.Text
  } deriving (Show, Generic)


instance FromJSON Admin where
  parseJSON = withObject "admin" $ \v -> do
    adminID'   <- v .: "id"
    adminName' <- v .: "name"

    pure Admin
      { adminID   = adminID'
      , adminName = adminName'
      }


nullAdmin :: Admin
nullAdmin = Admin
  { adminID   = AdminID "null"
  , adminName = "null"
  }
