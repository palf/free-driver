{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Drive.Intercom.Types.Users
  where

import           Data.Aeson
import           Data.Text                       (Text)
import qualified Drive.Intercom.Types.Pagination as P
import           GHC.Generics


newtype UserID
  = UserID Text
  deriving (Show, Generic, FromJSON)


data UsersResponse = UsersResponse
  { uPages :: Maybe P.Pagination
  , users  :: [User]
  } deriving (Show, Generic)

instance FromJSON UsersResponse where
  parseJSON = withObject "usersResponse" $ \v -> do
    uPages' <- v .: "pages"
    users'  <- v .: "users"

    pure UsersResponse
      { uPages = uPages'
      , users  = users'
      }


data User = User
  { cID    :: UserID
  , userID :: UserID
  , name   :: String
  } deriving (Show, Generic)


instance FromJSON User where
  parseJSON = withObject "customer" $ \v -> do
    cID'    <- v .: "id"
    userID' <- v .: "user_id"
    -- name    <- v .: "name"

    pure User
      { cID    = cID'
      , userID = userID'
      , name   = "user"
      }


nullUser :: User
nullUser = User
  { cID    = UserID "null"
  , userID = UserID "null"
  , name   = "null"
  }


-- data ScrollResponse = ScrollResponse
--   { scrollParam :: Text
--   , users       :: [User]
--   } deriving (Show, Generic)


-- instance FromJSON ScrollResponse
