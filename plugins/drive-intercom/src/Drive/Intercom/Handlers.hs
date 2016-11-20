{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Drive.Intercom.Handlers
  ( intercomToDescribeI
  , intercomToNetworkI
  , IntercomError (..)
  ) where

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Drive              as D
import qualified Drive.HTTP         as D
import qualified Drive.Describe          as D
import qualified Network.Wreq       as W

import Drive.Intercom.Types
import Control.Lens
import Data.Aeson
import Data.Monoid ((<>))


intercomToDescribeI :: D.Interpreter IntercomF D.DescribeF a
intercomToDescribeI (ListUsers a)
  = a [] <$ D.debug "listing customers"

intercomToDescribeI (ListAdmins a)
  = a [] <$ D.debug "listing admins"

intercomToDescribeI (ListConversations a)
  = a [] <$ D.debug "listing conversations"

intercomToDescribeI (GetUser (UserID u) a)
  = a nullUser <$ D.debug ("requesting user (" <> u <> ")")

intercomToDescribeI (GetAdmin (AdminID u) a)
  = a nullAdmin <$ D.debug ("requesting user (" <> u <> ")")

intercomToDescribeI (GetConversation (ConversationID c) a)
  = a nullConversation <$ D.debug ("requesting conversation (" <> c <> ")")


mkOpts :: IntercomCredentials -> W.Options
mkOpts y
  = W.defaults
    & W.header "Accept" .~ ["application/json"]
    & W.header "Authorization" .~ [T.encodeUtf8 (authorization y)]


newtype IntercomError
  = IntercomError String
  deriving (Show, Eq)


intercomToNetworkI
  :: D.Interpreter IntercomF (D.HttpHeaderF IntercomCredentials) a
-- TODO: parse for errors (https://developers.intercom.com/reference#error-objects)
-- TODO: read rate limit
-- TODO: use the `scroll` endpoint

intercomToNetworkI (ListUsers a) = do
  r <- D.getRawOpts mkOpts "https://api.intercom.io/users"
  let r' = eitherDecode r :: Either String UsersResponse
  pure . a $ case r' of
    Left _e -> []
    Right v -> map userID (users v)

intercomToNetworkI (ListAdmins a) = do
  r <- D.getRawOpts mkOpts "https://api.intercom.io/admins"
  let r' = eitherDecode r :: Either String AdminsResponse
  pure . a $ case r' of
    Left _e  -> []
    Right v -> admins v

intercomToNetworkI (ListConversations a) = do
  r <- D.getRawOpts mkOpts "https://api.intercom.io/conversations"
  let r' = eitherDecode r :: Either String ConversationsResponse
  pure . a $ case r' of
    Left _e -> []
    Right v -> map convID (conversations v)

intercomToNetworkI (GetUser (UserID u) a) = do
  r <- D.getRawOpts mkOpts ("https://api.intercom.io/users/" <> T.unpack u)
  let r' = eitherDecode r :: Either String User
  pure . a $ case r' of
    Left _e -> nullUser
    Right v -> v

intercomToNetworkI (GetAdmin (AdminID u) a) = do
  r <- D.getRawOpts mkOpts ("https://api.intercom.io/admins/" <> T.unpack u)
  let r' = eitherDecode r :: Either String Admin
  pure . a $ case r' of
    Left _e -> nullAdmin
    Right v -> v

intercomToNetworkI (GetConversation (ConversationID c) a) = do
  r <- D.getRawOpts mkOpts ("https://api.intercom.io/conversations/" <> T.unpack c)
  let r' = eitherDecode r :: Either String Conversation
  pure . a $ case r' of
    Left _e -> nullConversation
    Right v -> v
