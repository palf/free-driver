{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Drive.Slack.Handlers
  ( slackToLog
  , execSlack
  , withSlackCredentials
  ) where


import Drive.Slack.Types

import qualified Data.Text              as Tx
import qualified Drive                  as D
import qualified Drive.Describe              as D
import qualified Web.Slack as S
import Data.Functor
import qualified Data.Yaml as Y


-- type SupportsSlack m = (IOC.MonadIO m)


-- execSlack :: (SupportsSlack m) => SlackF a -> m a
-- execSlack (SlackEntry l t a) = IOC.liftIO $ printMessage l t >> pure a


data SlackCredentialsError
  = NotFound
  deriving (Show)


slackToLog :: SlackF a -> D.Free D.DescribeF a
slackToLog (Ping a) = D.debug "ping" $> a
slackToLog _ = undefined


execSlack :: SlackF a -> IO a
execSlack = undefined


withSlackCredentials
  :: FilePath
  -> (S.SlackConfig -> IO a)
  -> IO (Either SlackCredentialsError a)

withSlackCredentials path f
  = getSlackConfig path
  >>= \x ->
      case x of
        Left e -> pure (Left e)
        Right r -> Right <$> f r
    -- case c of
    --   Nothing -> pure ()
    --   Just x -> f x


getSlackConfig :: FilePath -> IO (Either SlackCredentialsError S.SlackConfig)
getSlackConfig path = do
  r <- getCreds
  case r of
    Just (x:_) -> pure (Right $ mkC x)
    _ -> pure (Left NotFound)

   where
     getCreds = Y.decodeFile path :: IO (Maybe [SlackCredentials])
     mkC r = S.SlackConfig { S._slackApiToken = Tx.unpack (token r) }

