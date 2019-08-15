{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Drive.Slack.Types
  ( SlackF (..)
  , SlackCredentials (..)
  , Target (..)
  , targetId
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
type UserName = Text
type ChannelID = Text


data Target
  = User UserID UserName
  | Channel ChannelID
  deriving (Show, Eq)


targetId :: Target -> Text
targetId (User t _)  = t
targetId (Channel t) = t


data SlackF a
  = Ping a
  | ListUsers ([Target] -> a)
  | SendMessage Target Text a
  deriving (Functor)
