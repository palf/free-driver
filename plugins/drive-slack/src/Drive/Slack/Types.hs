{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Drive.Slack.Types
  ( SlackF (..)
  , SlackCredentials (..)
  , Target (..)
  ) where

import           Data.Text    (Text)
import qualified Data.Yaml    as Y
import qualified GHC.Generics as G


data SlackCredentials = SlackCredentials
  { name  :: Text
  , token :: Text
  } deriving (Show, Eq, G.Generic)

instance Y.FromJSON SlackCredentials


type UserID = Text
type ChannelID = Text


data Target
  = User UserID
  | Channel ChannelID


data SlackF a
  = Ping a
  | SendMessage Target Text a
  | RespondAll (Text -> a)
  | Respond Target (Text -> a)
  deriving (Functor)
