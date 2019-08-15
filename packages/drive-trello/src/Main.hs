{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators    #-}

module Main
  ( main
  ) where

import           Control.Monad.Free     (Free (..))
import           Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Reader   as R
import           Data.Text              (Text)
import qualified Data.Yaml              as Y
import qualified Drive                  as V
import Drive.Describe
import Drive.HTTP
import Drive.Trello


type HttpTrelloP = Free (HttpUriF TrelloAuth)


program :: TrelloP [Text]
program =
  fmap boardName <$> getBoards (User "jackpalfrey3")


asDebug :: TrelloP a -> DescribeP a
asDebug = V.foldFree trelloToDescribeI


asHttp :: TrelloP a -> HttpTrelloP (Either TrelloError a)
asHttp = V.foldEitherFree trelloToNetworkI


httpAsDescribe :: TrelloAuth -> HttpTrelloP a -> DescribeP a
httpAsDescribe auth = V.foldFree (httpUriToDescribe auth)


asVerbose
  :: TrelloP a
  -> R.ReaderT TrelloAuth DescribeP (Either TrelloError a)

asVerbose p
  = R.ask >>= \auth -> R.lift $ httpAsDescribe auth $ asHttp p


runDescribe :: (MonadIO m) => DescribeP a -> m a
runDescribe = V.foldFree execDescribe


runDescribeR :: (MonadIO m, R.MonadReader r m) => R.ReaderT r DescribeP a -> m a
runDescribeR p = R.ask >>= V.foldFree execDescribe . R.runReaderT p


runHttp
  :: (MonadIO m, R.MonadReader TrelloAuth m)
  => HttpTrelloP a
  -> m (Either HttpError a)

runHttp = V.foldEitherFree execHttpUri


main :: IO ()
main = do
  describe program >>= print

  withTrelloCredentials "credentials/trello.yaml" $ \auth -> do
    trelloVerboseDescribe auth
    trelloInterleavedDescribe auth
    trelloExecuting auth
    trelloExecutingInterleavedDescribe auth

  where
    describe :: (MonadIO m) => TrelloP a -> m a
    describe = runDescribe . asDebug


withTrelloCredentials :: FilePath -> (TrelloAuth -> IO a) -> IO ()
withTrelloCredentials p f =
  Y.decodeFileEither p >>=
    either print (\x -> f x >> pure ())




trelloVerboseDescribe :: TrelloAuth -> IO ()
trelloVerboseDescribe auth =
  R.runReaderT (run program) auth >>= print

    where
      run
        :: (MonadIO m, R.MonadReader TrelloAuth m)
        => TrelloP a
        -> m (Either TrelloError a)

      run = runDescribeR . asVerbose


trelloInterleavedDescribe :: TrelloAuth -> IO ()
trelloInterleavedDescribe auth =
  R.runReaderT (run program) auth >>= print

    where
      run
        :: (MonadIO m, R.MonadReader TrelloAuth m)
        => TrelloP a
        -> m (a, Either TrelloError a)

      run p = do
        r1 <- runDescribe $ asDebug p
        r2 <- runDescribeR $ asVerbose p
        pure (r1, r2)


trelloExecuting :: TrelloAuth -> IO ()
trelloExecuting auth =
  R.runReaderT (run program) auth >>= print

    where
      run
        :: (MonadIO m, R.MonadReader TrelloAuth m)
        => TrelloP a
        -> m (Either HttpError (Either TrelloError a))

      run = runHttp . asHttp


trelloExecutingInterleavedDescribe :: TrelloAuth -> IO ()
trelloExecutingInterleavedDescribe auth =
  R.runReaderT (run program) auth >>= print

    where
      run
        :: (MonadIO m, R.MonadReader TrelloAuth m)
        => TrelloP a
        -> m (a, Either TrelloError a, Either HttpError (Either TrelloError a))

      run p = do
        r1 <- runDescribe $ asDebug p
        r2 <- runDescribeR $ asVerbose p
        r3 <- runHttp $ asHttp p
        pure (r1, r2, r3)
