{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Reader as R
import qualified Data.Yaml            as Y
import qualified Drive                as D
import qualified Drive.HTTP           as H
import qualified Drive.Describe            as D
import qualified Drive.Trello         as T

import Control.Monad.IO.Class (MonadIO)
import Control.Monad      (void)
import Control.Monad.Free (Free(..))
import Data.Text          (Text)


type DescribeP   = Free D.DescribeF
type HttpTrelloP = Free (H.HttpUriF T.TrelloAuth)
type TrelloP     = Free T.TrelloF
type HError      = H.HttpError
type TError      = T.TrelloError


exampleGetTrelloBoardNames :: TrelloP [Text]
exampleGetTrelloBoardNames = do
  let user = T.User "jackpalfrey3"
  (fmap T.boardName ) <$> T.getBoards user


asDebug :: TrelloP a -> DescribeP a
asDebug = D.foldFree T.trelloToDescribeI


asHttp :: TrelloP a -> HttpTrelloP (Either TError a)
asHttp = D.foldEitherFree T.trelloToNetworkI


httpAsDescribe :: T.TrelloAuth -> HttpTrelloP a -> DescribeP a
httpAsDescribe auth = D.foldFree (H.httpUriToDescribe auth)


asVerbose
  :: TrelloP a
  -> R.ReaderT T.TrelloAuth DescribeP (Either TError a)

asVerbose p
  = R.ask >>= \auth -> R.lift $ httpAsDescribe auth $ asHttp p


runDescribe :: (MonadIO m) => DescribeP a -> m ()
runDescribe = void . D.foldFree D.execDescribe


runDescribeR :: (MonadIO m, R.MonadReader r m) => R.ReaderT r DescribeP a -> m ()
runDescribeR p = R.ask >>= void . D.foldFree D.execDescribe . R.runReaderT p


runHttp
  :: (MonadIO m, R.MonadReader T.TrelloAuth m)
  => HttpTrelloP a
  -> m (Either HError a)

runHttp = D.foldEitherFree H.execHttpUri


main :: IO ()
main = do
  trelloDebugDescribe

  withTrelloCredentials "credentials/trello.yaml" $ \auth -> do
    trelloVerboseDescribe auth
    trelloInterleavedDescribe auth
    trelloExecuting auth
    trelloExecutingInterleavedDescribe auth


withTrelloCredentials :: FilePath -> (T.TrelloAuth -> IO a) -> IO ()
withTrelloCredentials p f =
  Y.decodeFileEither p >>=
    either
      print
      (void . f)


trelloDebugDescribe :: IO ()
trelloDebugDescribe =
  run exampleGetTrelloBoardNames >>= print

    where
      run :: (MonadIO m) => TrelloP a -> m ()
      run = runDescribe . asDebug


trelloVerboseDescribe :: T.TrelloAuth -> IO ()
trelloVerboseDescribe auth =
  R.runReaderT (run exampleGetTrelloBoardNames) auth >>= print

    where
      run
        :: (MonadIO m, R.MonadReader T.TrelloAuth m)
        => TrelloP a
        -> m ()

      run = runDescribeR . asVerbose


trelloInterleavedDescribe :: T.TrelloAuth -> IO ()
trelloInterleavedDescribe auth =
  R.runReaderT (run exampleGetTrelloBoardNames) auth >>= print

    where
      run
        :: (MonadIO m, R.MonadReader T.TrelloAuth m)
        => TrelloP a
        -> m ((), ())

      run p = do
        r1 <- runDescribe $ asDebug p
        r2 <- runDescribeR $ asVerbose p
        pure (r1, r2)


trelloExecuting :: T.TrelloAuth -> IO ()
trelloExecuting auth =
  R.runReaderT (run exampleGetTrelloBoardNames) auth >>= print

    where
      run
        :: (MonadIO m, R.MonadReader T.TrelloAuth m)
        => TrelloP a
        -> m (Either HError (Either TError a))

      run = runHttp . asHttp


trelloExecutingInterleavedDescribe :: T.TrelloAuth -> IO ()
trelloExecutingInterleavedDescribe auth =
  R.runReaderT (run exampleGetTrelloBoardNames) auth >>= print

    where
      run
        :: (MonadIO m, R.MonadReader T.TrelloAuth m)
        => TrelloP a
        -> m ((), (), Either HError (Either TError a))

      run p = do
        r1 <- runDescribe $ asDebug p
        r2 <- runDescribeR $ asVerbose p
        r3 <- runHttp $ asHttp p
        pure (r1, r2, r3)
