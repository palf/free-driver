{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Drive.Intercom.Types
  ( IntercomF (..)
  , IntercomCredentials (..)
  , emptyCredentials

  , module P
  , module A
  , module U
  , module C
  ) where

import           Data.Aeson
import           Data.Text                         (Text)
import           Drive.Intercom.Types.Admins       as A
import           Drive.Intercom.Types.Conversation as C
import           Drive.Intercom.Types.Pagination   as P
import           Drive.Intercom.Types.Users        as U
import           GHC.Generics


data IntercomF a
  = ListUsers ([UserID] -> a)
  | ListAdmins ([Admin] -> a)
  | ListConversations ([ConversationID] -> a)
  | GetUser UserID (User -> a)
  | GetAdmin AdminID (Admin -> a)
  | GetConversation ConversationID (Conversation -> a)
  deriving (Functor)


newtype IntercomCredentials = IntercomCredentials
  { authorization :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON IntercomCredentials

emptyCredentials :: IntercomCredentials
emptyCredentials = IntercomCredentials
  { authorization = mempty
  }
