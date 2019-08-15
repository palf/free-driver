{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Drive.Slack.Handlers
  ( slackToDescribeI
  , execSlack
  , withSlackCredentials
  , SlackCredentialsError (..)
  , CanSlack
  ) where

import qualified Control.Monad.Reader   as R
import qualified Data.Text              as Text
import qualified Data.Yaml              as Y
import qualified Drive                  as D
import qualified Drive.Describe         as D
import qualified Web.Slack              as Slack
-- import qualified Web.Slack.Api          as Api
-- import qualified Web.Slack.Auth         as Auth
-- import qualified Web.Slack.Channel      as Channel
import qualified Web.Slack.Chat         as Chat
import qualified Web.Slack.Common       as Common
-- import qualified Web.Slack.Im           as Im
import qualified Web.Slack.User         as User

import           Control.Monad.IO.Class (MonadIO)
import           Data.Functor           ((<$))
import           Data.Text              (Text)
import           Drive.Slack.Types


newtype SlackCredentialsError
  = NotFound String
  deriving (Show)


getSlackConfig :: FilePath -> IO (Either SlackCredentialsError Slack.SlackConfig)
getSlackConfig path = do
  x <- Y.decodeFileEither path
  either
    (pure . Left . NotFound . show)
    (\v -> Right <$> Slack.mkSlackConfig (token v))
    x


slackToDescribeI :: SlackF a -> D.Free D.DescribeF a
slackToDescribeI (Ping a)            = a    <$ D.debug "ping"
slackToDescribeI (ListUsers a)       = a [] <$ D.debug "list users"
slackToDescribeI (SendMessage t m a) = a    <$ D.debug ("to " <> Text.pack (show t) <> " -> sending message \"" <> m <> "\"")


withSlackCredentials
  :: FilePath
  -> (Slack.SlackConfig -> IO a)
  -> IO (Either SlackCredentialsError a)

withSlackCredentials path f
  = getSlackConfig path
  >>= either (pure . Left) (fmap Right . f)


type CanSlack env m =
  ( MonadIO m
  , R.MonadReader env m
  , Slack.HasManager env
  , Slack.HasToken env
  )


execSlack :: (CanSlack env m) => SlackF a -> m a
execSlack (Ping a)            = a <$  fishPing
execSlack (ListUsers a)       = a <$> fishListUsers
execSlack (SendMessage t m a) = a <$  fishSendMessage t m


fishPing :: (CanSlack env m) => m ()
fishPing = undefined


fishListUsers :: (CanSlack env m) => m [Target]
fishListUsers = h <$> Slack.usersList
  where
    h :: Slack.Response User.ListRsp -> [Target]
    h = either (const []) (fmap asTarget . User.listRspMembers)

    asTarget :: User.User -> Target
    asTarget x = User (Common.unUserId $ User.userId x) (User.userName x)


fishSendMessage :: (CanSlack env m) => Target -> Text -> m (Slack.Response Chat.PostMsgRsp)
fishSendMessage t m =
  Slack.chatPostMessage (Chat.mkPostMsgReq (targetId t) m)

-- run :: IO (Either Slack.SlackCredentialsError
--   ( Slack.Response Api.TestRsp
--   , Slack.Response Auth.TestRsp
--   , Maybe [Text]
--   , Maybe [(Text, Text)]
--   , Slack.Response Chat.PostMsgRsp
--   ) )
-- run =
--   Slack.withSlackCredentials "credentials/slack.yaml" $ R.runReaderT $ do
--     (\a b c d e -> ( a, b, c, d, e ))
--     <$> Slack.apiTest Api.mkTestReq
--     <*> Slack.authTest
--     <*> (f <$> Slack.channelsList Channel.mkListReq)
--     -- <*> (g <$> Slack.imList)
--     <*> (h <$> Slack.usersList)

--   where
--     f :: Slack.Response Channel.ListRsp -> Maybe [Text]
--     f = either
--         (const Nothing)
--         (\res -> Just $ Channel.channelName <$> Channel.listRspChannels res)

-- --     g :: Slack.Response Im.ListRsp -> Maybe [(Text, Text)]
-- --     g = either
-- --         (const Nothing)
-- --         (\res -> Just $ (\x -> (Im.imId x, Text.pack $ show $ Common.unUserId $ Im.imUser x)) <$> Im.listRspIms res)
