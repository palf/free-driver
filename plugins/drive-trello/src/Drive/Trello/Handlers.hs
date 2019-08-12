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
import qualified Drive              as D
import qualified Drive.Describe     as D
import qualified Drive.HTTP         as H
import           Drive.Trello.Types
import qualified Network.Wreq       as W


type DescribeP   = D.Free D.DescribeF
type HttpTrelloP = D.Free (H.HttpUriF TrelloAuth)


data TrelloError
  = DecodeError String
  | RequestFailed
  deriving (Show)


trelloToDescribeI :: TrelloF a -> DescribeP a
trelloToDescribeI (GetBoards u f)
  = D.debug ("getting boards for user \"" <> showUser u <> "\"") $> f []


showUser :: User -> Text
showUser (User u) = u


opts :: W.Options
opts
  = W.defaults
  & W.header "Accept" .~ ["application/json"]

--liftError :: Contains es e => e -> Either es a

trelloToNetworkI :: TrelloF a -> HttpTrelloP (Either TrelloError a)
trelloToNetworkI (GetBoards u f) = do
  r <- H.getRawUrl opts (getPath u)
  case r of
    Left _e -> pure $ Left RequestFailed
    Right v -> do
      let r' = eitherDecode v -- :: Either String [Board]
      case r' of
        Left _e' -> pure $ Left (DecodeError $ show v)
        Right v' -> pure $ Right (f v')

  --f <$> mapEitherT (injectToSum....) $ liftEitherT $ eitherDecode =<< H.getRawUrl opts (getPath u)


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
