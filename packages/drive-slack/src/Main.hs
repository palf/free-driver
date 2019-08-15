{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Reader as R
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Drive
import           Drive.Describe
import           Drive.Slack


program :: SlackP Text
program = do
  users <- listUsers
  let me = filter (hasName "palfrey") users
  case me of
    [x] -> sendMessage x "hello" >> pure "sent message"
    _   -> pure "no users found"

  where
    hasName :: Text -> Target -> Bool
    hasName s (User _ x) = s `Text.isInfixOf` x
    hasName _ _          = False


main :: IO ()
main = do
  describe program >>= print

  withSlackCredentials "credentials/slack.yaml" $ \creds -> do
    R.runReaderT (run program) creds >>= print
  >>= print

  where
    describe
      = slackToDescribeI >---> execDescribe

    run :: (CanSlack env m) => SlackP a -> m a
    run = identityI >---> execSlack

