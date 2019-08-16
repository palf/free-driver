{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
  ( main
  , program
  , run
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Reader   as R
import           Data.Text              (Text)
import qualified Data.Yaml              as Y
import qualified Drive                  as V
import Drive
import           Drive.Describe
import           Drive.HTTP
import Control.Monad.Except
import           Drive.Trello


program :: TrelloP [Text]
program = do
  fmap boardName <$> getBoards (User "jackpalfrey3")
  fmap boardName <$> getBoards (User "jackpalfrey3")
  fmap boardName <$> getBoards (User "jackpalfrey3")


withTrelloCredentials :: FilePath -> R.ReaderT TrelloAuth IO a -> IO (Either Y.ParseException a)
withTrelloCredentials p f =
  Y.decodeFileEither p >>=
    either (pure . Left) (\x -> Right <$> R.runReaderT f x)


run = withTrelloCredentials "credentials/trello.yaml"


main :: IO ()
main = do
  -- describe program >>= print

  withTrelloCredentials "credentials/trello.yaml" $ do
    -- describeVerbose program >>= liftIO . print
    describeBoth program >>= liftIO . print
    -- runSilent program >>= liftIO . print
    -- runDescribe program >>= liftIO . print
  >>= print

  where
    describe :: (MonadIO m) => TrelloP a -> m a
    describe = trelloToDescribeI >---> execDescribe


    describeVerbose :: (R.MonadReader TrelloAuth m, MonadIO m) => TrelloP a -> m (Either TrelloError a)
    describeVerbose p = do
      auth <- R.ask
      ((httpUriToDescribe auth >---> execDescribe)
        . runExceptT
        . foldFree (ExceptT . trelloToNetworkI)) p

      where
        -- f :: (MonadIO m) => TrelloAuth -> HttpUriP TrelloAuth b -> m b
        -- f auth =

--         k
--           :: (forall x. TrelloF x -> ExceptT TrelloError (Free (HttpUriF TrelloAuth)) x)
--           -> TrelloP a
--           -> HttpUriP TrelloAuth (Either TrelloError a)

--         k q = runExceptT . foldFree q


    describeBoth :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> m (Either TrelloError a)
    describeBoth = undefined
--     describeBoth
--       = (trelloToDescribeI <---> V.identityI) >---> ((fmap Right execDescribe) >---< describeVerbose)


    runSilent :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> m (Either HttpError (Either TrelloError a))
    runSilent = V.foldEitherFree execHttpUri . V.foldEitherFree trelloToNetworkI


    runDescribe :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> m (a, Either TrelloError a, Either HttpError (Either TrelloError a))
    runDescribe p = do
      r1 <- describe p
      r2 <- describeVerbose p
      r3 <- (V.foldEitherFree execHttpUri . V.foldEitherFree trelloToNetworkI) p
      pure (r1, r2, r3)
