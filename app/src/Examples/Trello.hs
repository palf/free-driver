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

import Control.Monad      (void)
import Control.Monad.Free (Free(..))
import Data.Monoid        ((<>))
import Data.Text          (Text)


type DescribeP        = D.Free D.DescribeF
type HttpTrelloP = D.Free (H.HttpUriF T.TrelloAuth)
type TrelloP     = D.Free T.TrelloF
type HError      = H.HttpError
type TError      = T.TrelloError


ff :: Monad m => (forall x. f x -> m x) -> D.Free f a -> m a
ff = D.foldFree


fe :: Monad m => (forall x. t x -> m (Either e x)) -> Free t a -> m (Either e a)
fe = D.foldEitherFree


header :: String -> IO ()
header t = putStrLn ("\n\n# " <> t)


subheader :: String -> IO ()
subheader t = putStrLn ("\n-- " <> t <> "\n")


exampleGetTrelloBoardNames :: TrelloP [Text]
exampleGetTrelloBoardNames = do
  let user = T.User "jackpalf"
  bs <- T.getBoards user
  pure $ fmap T.boardName bs


asDebug :: TrelloP a -> DescribeP a
asDebug = ff T.trelloToDescribeI


asHttp :: TrelloP a -> HttpTrelloP (Either TError a)
asHttp = fe T.trelloToNetworkI


httpAsDescribe :: T.TrelloAuth -> HttpTrelloP a -> DescribeP a
httpAsDescribe auth = ff (H.httpUriToDescribe auth)


asVerbose
  :: TrelloP a
  -> R.ReaderT T.TrelloAuth DescribeP (Either TError a)

asVerbose p
  = R.ask >>= \auth -> R.lift $ httpAsDescribe auth $ asHttp p


runDescribe :: (R.MonadIO m) => DescribeP a -> m ()
runDescribe = void . ff D.execDescribe


runDescribeR :: (R.MonadIO m, R.MonadReader r m) => R.ReaderT r DescribeP a -> m ()
runDescribeR p = R.ask >>= void . ff D.execDescribe . R.runReaderT p


runHttp
  :: (R.MonadIO m, R.MonadReader T.TrelloAuth m)
  => HttpTrelloP a
  -> m (Either HError a)

runHttp = fe H.execHttpUri


main :: IO ()
main = do
  header "Trello"

  trelloDebugDescribe

  withTrelloCredentials "credentials/trello.yaml" $ \auth -> do
    trelloVerboseDescribe auth
    trelloInterleavedDescribe auth
    trelloExecuting auth
    trelloExecutingInterleavedDescribe auth


withTrelloCredentials :: FilePath -> (T.TrelloAuth -> IO a) -> IO ()
withTrelloCredentials p f
  = Y.decodeFile p >>= maybe (putStrLn "could not load trello credentials") (void . f)


trelloDebugDescribe :: IO ()
trelloDebugDescribe = do
  subheader "describe (debug)"
  run exampleGetTrelloBoardNames >>= print

    where
      run :: (R.MonadIO m) => TrelloP a -> m ()
      run = runDescribe . asDebug


trelloVerboseDescribe :: T.TrelloAuth -> IO ()
trelloVerboseDescribe auth = do
  subheader "describe (verbose)"
  R.runReaderT (run exampleGetTrelloBoardNames) auth >>= print

    where
      run
        :: (R.MonadIO m, R.MonadReader T.TrelloAuth m)
        => TrelloP a
        -> m ()

      run = runDescribeR . asVerbose


trelloInterleavedDescribe :: T.TrelloAuth -> IO ()
trelloInterleavedDescribe auth = do
  subheader "describe (interleaved)"
  R.runReaderT (run exampleGetTrelloBoardNames) auth >>= print

    where
      run
        :: (R.MonadIO m, R.MonadReader T.TrelloAuth m)
        => TrelloP a
        -> m ((), ())

      run p = do
        r1 <- runDescribe $ asDebug p
        r2 <- runDescribeR $ asVerbose p
        pure (r1, r2)


trelloExecuting :: T.TrelloAuth -> IO ()
trelloExecuting auth = do
  subheader "executing"
  R.runReaderT (run exampleGetTrelloBoardNames) auth >>= print

    where
      run
        :: (R.MonadIO m, R.MonadReader T.TrelloAuth m)
        => TrelloP a
        -> m (Either HError (Either TError a))

      run = runHttp . asHttp


trelloExecutingInterleavedDescribe :: T.TrelloAuth -> IO ()
trelloExecutingInterleavedDescribe auth = do
  subheader "executing with interleaved describe"
  R.runReaderT (run exampleGetTrelloBoardNames) auth  >>= print

    where
      run
        :: (R.MonadIO m, R.MonadReader T.TrelloAuth m)
        => TrelloP a
        -> m ((), (), Either HError (Either TError a))

      run p = do
        r1 <- runDescribe $ asDebug p
        r2 <- runDescribeR $ asVerbose p
        r3 <- runHttp $ asHttp p
        pure (r1, r2, r3)
