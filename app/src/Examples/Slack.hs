{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

-- import qualified Drive                as D
-- import qualified Drive.Describe            as D
import qualified Drive.Slack       as S
import           Web.Slack
import           Web.Slack.Message


-- ff :: Monad m => (forall x. f x -> m x) -> D.Free f a -> m a
-- ff = D.foldFree


main :: IO ()
main = do
  r <- S.withSlackCredentials "credentials/slack.yaml" $ \c ->
    runBot c echoBot ()
  print r


-- type SlackBot s = Event -> Slack s ()
echoBot :: SlackBot ()
echoBot (Message cid _ msg _ _ _) = sendMessage cid msg
echoBot _                         = return ()
