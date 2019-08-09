{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Drive.Slack.Types
  ( SlackF (..)
  , SlackCredentials (..)
  , Target (..)
  ) where

import qualified Data.Text    as Tx
import qualified Data.Yaml    as Y
import qualified GHC.Generics as G
-- import qualified Web.Slack         as S
-- import qualified Web.Slack.Message as S.M


data SlackCredentials = SlackCredentials
  { name  :: Tx.Text
  , token :: Tx.Text
  } deriving (Show, Eq, G.Generic)

instance Y.FromJSON SlackCredentials


type UserID = Tx.Text
type ChannelID = Tx.Text


data Target
  = User UserID
  | Channel ChannelID


data SlackF a
  = Ping a
  | SendMessage Target Tx.Text a
  | RespondAll (Tx.Text -> a)
  | Respond Target (Tx.Text -> a)
  deriving (Functor)
