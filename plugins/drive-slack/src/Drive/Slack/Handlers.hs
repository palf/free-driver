{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Drive.Slack.Handlers
  ( slackToLog
  , execSlack
  , withSlackCredentials
  ) where


import           Data.Functor      (($>))
import qualified Data.Text         as Text
import qualified Data.Yaml         as Y
import qualified Drive             as D
import qualified Drive.Describe    as D
import           Drive.Slack.Types
import qualified Web.Slack         as S


data SlackCredentialsError
  = NotFound
  deriving (Show)


slackToLog :: SlackF a -> D.Free D.DescribeF a
slackToLog (Ping a) = D.debug "ping" $> a
slackToLog _        = undefined


execSlack :: SlackF a -> IO a
execSlack = undefined


withSlackCredentials
  :: FilePath
  -> (S.SlackConfig -> IO a)
  -> IO (Either SlackCredentialsError a)

withSlackCredentials path f
  = getSlackConfig path
  >>= either
    (pure . Left)
    (fmap Right . f)


getSlackConfig :: FilePath -> IO (Either SlackCredentialsError S.SlackConfig)
getSlackConfig path = f <$> Y.decodeFileEither path

  where
    f :: Either Y.ParseException [SlackCredentials] -> Either SlackCredentialsError S.SlackConfig
    f (Right (x:_)) = Right (mkC x)
    f _             = Left NotFound

    mkC r = S.SlackConfig { S._slackApiToken = Text.unpack (token r) }
