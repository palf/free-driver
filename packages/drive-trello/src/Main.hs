{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Main
  ( main
  , program
  , run
  ) where

import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Reader   as R
import           Data.Text              (Text)
import qualified Data.Yaml              as Y
import           Drive
import qualified Drive                  as V
import           Drive.Describe
import           Drive.HTTP
import           Drive.Trello


program :: TrelloP [Text]
program = do
  fmap boardName <$> getBoards (User "jackpalfrey3")
  fmap boardName <$> getBoards (User "jackpalfrey3")
  fmap boardName <$> getBoards (User "jackpalfrey3")


withTrelloCredentials :: FilePath -> R.ReaderT TrelloAuth IO a -> IO (Either Y.ParseException a)
withTrelloCredentials p f =
  Y.decodeFileEither p >>=
    either (pure . Left) (fmap Right . R.runReaderT f)


run = withTrelloCredentials "credentials/trello.yaml"


main :: IO ()
main = do
  describe program >>= print

  withTrelloCredentials "credentials/trello.yaml" $ do
    runExceptT (describeVerbose program) >>= liftIO . print
    runExceptT (describeBoth program) >>= liftIO . print
    -- runSilent program >>= liftIO . print
    -- runDescribe program >>= liftIO . print
  >>= print

  where
    describe :: (MonadIO m) => TrelloP a -> m a
    describe = trelloToDescribeI >---> execDescribe


    -- describeVerbose :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> m (Either TrelloError a)
    describeVerbose :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> ExceptT TrelloError m a
    describeVerbose p = do
      auth <- lift R.ask
      (z auth  . foldFree g) p

      where
         describeReq :: (MonadIO m) => t -> HttpUriP t b -> m b
         describeReq x = httpUriToDescribe x >---> execDescribe

         z :: (MonadIO m) => t -> ExceptT e (HttpUriP t) b -> ExceptT e m b
         z = mapExceptT . describeReq

         g :: TrelloF a -> ExceptT TrelloError (HttpUriP TrelloAuth) a
         g = ExceptT . trelloToNetworkI


    describeBoth :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> ExceptT TrelloError m a
    describeBoth p = do
      auth <- lift R.ask

      ( foldFree (\t -> case t of ; (L x) -> d2io x ; (R x) -> d2io x)
        . foldFree (\c -> liftL (t2d c) >> liftR (t2v c))
        ) p


    runSilent :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> m (Either HttpError (Either TrelloError a))
    runSilent = V.foldEitherFree execHttpUri . V.foldEitherFree trelloToNetworkI


--     runDescribe :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> m (a, Either TrelloError a, Either HttpError (Either TrelloError a))
--     runDescribe p = do
--       r1 <- describe p
--       r2 <- describeVerbose p
--       r3 <- (V.foldEitherFree execHttpUri . V.foldEitherFree trelloToNetworkI) p
--       pure (r1, r2, r3)
--
t2t :: TrelloF a -> Free TrelloF a
t2t = identityI
t2d :: TrelloF a -> Free DescribeF a
t2d = trelloToDescribeI
t2h :: TrelloF a -> ExceptT TrelloError (Free (HttpUriF TrelloAuth)) a
t2h = ExceptT . trelloToNetworkI
t2io :: TrelloF a -> ExceptT TrelloError IO a
t2io = undefined
t2v :: TrelloAuth -> TrelloF a -> ExceptT TrelloError (Free DescribeF) a
t2v auth p = _ (t2h p)  (foldFree (h2d auth))
h2d :: TrelloAuth -> HttpUriF TrelloAuth a -> Free DescribeF a
h2d = httpUriToDescribe
h2io :: (MonadIO m, R.MonadReader x m) => HttpUriF x a -> m (Either HttpError a)
h2io = execHttpUri
d2io :: DescribeF a -> IO a
d2io = execDescribe

t2tf :: Free TrelloF a -> Free TrelloF a
t2tf = foldFree identityI
t2df :: Free TrelloF a -> Free DescribeF a
t2df = foldFree trelloToDescribeI
t2hf :: Free TrelloF a -> ExceptT TrelloError (Free (HttpUriF TrelloAuth)) a
t2hf = foldFree (ExceptT . trelloToNetworkI)
t2iof :: Free TrelloF a -> IO a
t2iof = foldFree undefined
h2df :: TrelloAuth -> Free ( HttpUriF TrelloAuth ) a -> Free DescribeF a
h2df auth = foldFree (httpUriToDescribe auth)
h2iof :: (MonadIO m, R.MonadReader x m) => Free (HttpUriF x) a -> ExceptT HttpError m a
h2iof = foldFree (ExceptT . execHttpUri)
d2iof :: Free DescribeF a -> IO a
d2iof = foldFree execDescribe

