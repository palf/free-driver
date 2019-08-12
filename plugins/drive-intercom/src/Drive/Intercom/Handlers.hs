{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Drive.Intercom.Handlers
  ( IntercomError (..)
  , intercomToDescribeI
  , intercomToNetworkI

  , listAllConversations
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.Bifunctor       as Bi
import           Data.Functor         (($>))
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Encoding   (encodeUtf8)
import qualified Drive                as D
import qualified Drive.Describe       as D
import qualified Drive.HTTP           as H
import           Drive.Intercom.Types
import qualified Network.Wreq         as W


type DescribeP     = D.Free D.DescribeF
type HttpIntercomP = D.Free (H.HttpHeaderF IntercomCredentials)


intercomToDescribeI :: IntercomF a -> DescribeP a
intercomToDescribeI (ListUsers a)
  = D.debug "listing customers" $> a []

intercomToDescribeI (ListAdmins a)
  = D.debug "listing admins" $> a []

intercomToDescribeI (ListConversations a)
  = D.debug "listing conversations" $> a []

intercomToDescribeI (GetUser (UserID u) a)
  = D.debug ("requesting user (" <> u <> ")") $> a nullUser

intercomToDescribeI (GetAdmin (AdminID u) a)
  = D.debug ("requesting user (" <> u <> ")") $> a nullAdmin

intercomToDescribeI (GetConversation (ConversationID c) a)
  = D.debug ("requesting conversation (" <> c <> ")") $> a nullConversation


mkOpts :: IntercomCredentials -> W.Options
mkOpts y
  = W.defaults
    & W.header "Accept" .~ ["application/json"]
    & W.header "Authorization" .~ [encodeUtf8 (authorization y)]


newtype IntercomError
  = IntercomError String
  deriving (Show, Eq)


intercomToNetworkI :: IntercomF a -> HttpIntercomP (Either IntercomError a)
-- TODO: parse for errors (https://developers.intercom.com/reference#error-objects)
-- TODO: read rate limit
-- TODO: use the `scroll` endpoint

intercomToNetworkI (ListUsers a)
  = fmap a <$> mkIntercomRequest "https://api.intercom.io/users"

intercomToNetworkI (ListAdmins a)
  = fmap a <$> mkIntercomRequest "https://api.intercom.io/admins"

intercomToNetworkI (ListConversations a)
  -- = fmap a <$> mkIntercomRequest "https://api.intercom.io/conversations"
  = fmap a <$> listAllConversations

intercomToNetworkI (GetUser (UserID u) a)
  = fmap a <$> mkIntercomRequest ("https://api.intercom.io/users/" <> u)

intercomToNetworkI (GetAdmin (AdminID u) a)
  = fmap a <$> mkIntercomRequest ("https://api.intercom.io/admins/" <> u)

intercomToNetworkI (GetConversation (ConversationID c) a)
  = fmap a <$> mkIntercomRequest ("https://api.intercom.io/conversations/" <> c)


listAllConversations :: HttpIntercomP (Either IntercomError [ConversationID])
listAllConversations
  = (fmap . fmap . fmap) convID (runit "https://api.intercom.io/conversations")

  where
    runit :: Text -> HttpIntercomP (Either IntercomError [Conversation])
    runit u =
      mkIntercomRequest u >>=
      either
        (pure . Left)
        (\v -> do
          let rs = conversations v
          maybe
            (pure $ Right rs)
            (fmap (mapRight (mappend rs)) . runit)
            (getNextPageLink v)
        )

    getNextPageLink :: ConversationsResponse -> Maybe Text
    getNextPageLink = pages >=> next


mkIntercomRequest
  :: (FromJSON a)
  => Text
  -> HttpIntercomP (Either IntercomError a)

mkIntercomRequest u
  = mapLeft IntercomError . eitherDecode
  <$> H.getRawOpts mkOpts (Text.unpack u)


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft = Bi.first

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight = Bi.second

-- mapOver :: (a -> c) -> (b -> d) -> Either a b -> Either c d
-- mapOver = Bi.bimap
