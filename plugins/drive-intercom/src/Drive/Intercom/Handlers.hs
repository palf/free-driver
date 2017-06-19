{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Drive.Intercom.Handlers
  ( IntercomError (..)
  , intercomToDescribeI
  , intercomToNetworkI
  ) where

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Drive              as D
import qualified Drive.Describe     as D
import qualified Drive.HTTP         as H
import qualified Network.Wreq       as W

import Drive.Intercom.Types
import Control.Lens
import Data.Aeson
import Data.Monoid ((<>))


type DescribeP     = D.Free D.DescribeF
type HttpIntercomP = D.Free (H.HttpHeaderF IntercomCredentials)


intercomToDescribeI :: IntercomF a -> DescribeP a
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


intercomToNetworkI :: IntercomF a -> HttpIntercomP (Either IntercomError a)
-- TODO: parse for errors (https://developers.intercom.com/reference#error-objects)
-- TODO: read rate limit
-- TODO: use the `scroll` endpoint

intercomToNetworkI (ListUsers a) = do
  r <- H.getRawOpts mkOpts "https://api.intercom.io/users"
  let r' = eitherDecode r :: Either String UsersResponse
  pure $ case r' of
    Left e -> Left (IntercomError e)
    Right v -> Right $ a $ map userID (users v)

intercomToNetworkI (ListAdmins a) = do
  r <- H.getRawOpts mkOpts "https://api.intercom.io/admins"
  let r' = eitherDecode r :: Either String AdminsResponse
  pure $ case r' of
    Left e  -> Left (IntercomError e)
    Right v -> Right $ a $ admins v

intercomToNetworkI (ListConversations a) = do
  r <- H.getRawOpts mkOpts "https://api.intercom.io/conversations"
  let r' = eitherDecode r :: Either String ConversationsResponse
  pure $ case r' of
    Left e -> Left (IntercomError e)
    Right v -> Right $ a $ map convID (conversations v)

intercomToNetworkI (GetUser (UserID u) a) = do
  r <- H.getRawOpts mkOpts ("https://api.intercom.io/users/" <> T.unpack u)
  let r' = eitherDecode r :: Either String User
  pure $ case r' of
    Left e -> Left (IntercomError e)
    Right v -> Right $ a v

intercomToNetworkI (GetAdmin (AdminID u) a) = do
  r <- H.getRawOpts mkOpts ("https://api.intercom.io/admins/" <> T.unpack u)
  let r' = eitherDecode r :: Either String Admin
  pure $ case r' of
    Left e -> Left (IntercomError e)
    Right v -> Right $ a v

intercomToNetworkI (GetConversation (ConversationID c) a) = do
  r <- H.getRawOpts mkOpts ("https://api.intercom.io/conversations/" <> T.unpack c)
  let r' = eitherDecode r :: Either String Conversation
  pure $ case r' of
    Left e -> Left (IntercomError e)
    Right v -> Right $ a v
