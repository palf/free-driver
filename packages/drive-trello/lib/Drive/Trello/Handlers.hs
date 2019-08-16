{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Drive.Trello.Handlers
  ( TrelloError (..)
  , trelloToDescribeI
  , trelloToNetworkI
  , opts
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Functor       (($>))
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as Text
import Drive.Describe
import Drive.HTTP
import           Drive.Trello.Types
import qualified Network.Wreq       as W


data TrelloError
  = DecodeError String
  | RequestFailed HttpError
  deriving (Show)


trelloToDescribeI :: TrelloF a -> DescribeP a
trelloToDescribeI (GetBoards u f)
  = debug ("getting boards for user \"" <> showUser u <> "\"") $> f []


showUser :: User -> Text
showUser (User u) = u


opts :: W.Options
opts
  = W.defaults
  & W.header "Accept" .~ ["application/json"]

--liftError :: Contains es e => e -> Either es a

trelloToNetworkI :: TrelloF a -> HttpUriP TrelloAuth (Either TrelloError a)
trelloToNetworkI (GetBoards u f) =
  either
    (Left . RequestFailed)
    (\v -> either
      (\_e' -> Left (DecodeError $ show v))
      (Right . f)
      (eitherDecode v)
    )
  <$> getRawUrl opts (getPath u)

  --f <$> mapExceptT (injectToSum....) $ liftExceptT $ eitherDecode =<< H.getRawUrl opts (getPath u)


getPath :: User -> TrelloAuth -> String
getPath (User u) auth
  = Text.unpack $ "https://api.trello.com/1/members/"
    <> u
    <> "/boards?"
    <> "token=" <> token auth
    <> "&key=" <> key auth


-- getPath' :: Organization -> TrelloAuth -> String
-- getPath' (Organization u) auth
--   = Text.unpack $ "https://api.trello.com/1/organizations/"
--     <> u
--     <> "/boards?"
--     <> "token=" <> token auth
--     <> "&key=" <> key auth
