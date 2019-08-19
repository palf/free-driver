{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Reader   as R
import qualified Data.Yaml              as Y
import qualified Drive                  as V

import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import           Drive
import           Drive.Describe
import           Drive.HTTP
import           Drive.Trello


program :: TrelloP [Text]
program = do
  _ <- fmap boardName <$> getBoards (User "jackpalfrey3")
  _ <- fmap boardName <$> getBoards (User "jackpalfrey3")
  fmap boardName <$> getBoards (User "jackpalfrey3")


withTrelloCredentials :: FilePath -> R.ReaderT TrelloAuth IO a -> IO (Either Y.ParseException a)
withTrelloCredentials p f =
  Y.decodeFileEither p >>=
    either (pure . Left) (fmap Right . R.runReaderT f)


main :: IO ()
main = do
  describe program >>= print

  withTrelloCredentials "credentials/trello.yaml" $ do
    runExceptT (describeVerbose program) >>= liftIO . print
    runExceptT (describeBoth program) >>= liftIO . print
    runSilent program >>= liftIO . print
    runExceptT (runExceptT (runDescribe program)) >>= liftIO . print
    pure ()
  >>= either print (const (print ("done" :: String)))

  where
    describe :: (MonadIO m) => TrelloP a -> m a
    describe = t2d >---> d2io


    describeVerbose :: (MonadIO m) => TrelloP a -> ExceptT TrelloError (R.ReaderT TrelloAuth m) a
    describeVerbose
      -- =  (\p' ->  k3 `R.mapReaderT` k2 p') `mapExceptT` k1 p
      =  mapExceptT (R.mapReaderT k3 . k2) . k1

      where
        k1 :: TrelloP a -> ExceptT TrelloError (HttpUriP TrelloAuth) a
        k1 = foldFree t2h
        k2 :: HttpUriP x a -> R.ReaderT x DescribeP a
        k2 = foldFree h2dr
        k3 :: (MonadIO m) => DescribeP a -> m a
        k3 = foldFree d2io


    describeBoth :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> ExceptT TrelloError m a
    describeBoth p = do
      auth <- lift R.ask

      ( f . g auth) p

      where
        f :: (MonadIO m) => ExceptT TrelloError (Free (DescribeF :+: DescribeF)) a -> ExceptT TrelloError m a
        f = mapExceptT (foldFree f1)

        f1 :: (MonadIO m) => (DescribeF :+: DescribeF) a -> m a
        f1 t = case t of ; (L x) -> d2io x ; (R x) -> d2io x

        g :: TrelloAuth -> TrelloP a -> ExceptT TrelloError (Free (DescribeF :+: DescribeF)) a
        g auth = ExceptT . (runExceptT . foldFree (ExceptT . k auth) )

        k :: TrelloAuth -> TrelloF a -> Free (DescribeF :+: DescribeF) (Either TrelloError a)
        k auth c = g1 c >> g2 auth c

        g1 :: TrelloF a -> Free (DescribeF :+: DescribeF) a
        g1 = liftL . t2d

        g2 :: TrelloAuth -> TrelloF a -> Free (DescribeF :+: DescribeF) (Either TrelloError a)
        g2 auth = liftR . runExceptT . t2v auth


    runSilent :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> m (Either HttpError (Either TrelloError a))
    runSilent = V.foldEitherFree execHttpUri . V.foldEitherFree trelloToNetworkI


    -- describe, verbose and run
    runDescribe :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> T m a
    runDescribe p = do
      auth <- lift R.ask

      ( f . g auth) p

      where
        f :: (MonadIO m, R.MonadReader TrelloAuth m) => ExceptT TrelloError (Free Triple) a -> T m a
        f = mapExceptT f'

        f' :: (MonadIO m, R.MonadReader TrelloAuth m) => Free Triple a -> ExceptT HttpError m a
        f' = foldFree f1

        f1 :: (MonadIO m, R.MonadReader TrelloAuth m) => Triple a -> ExceptT HttpError m a
        f1 t
          = case t of
              (L x)     -> d2io x
              (R (L x)) -> d2io x
              (R (R x)) -> h2io x

        g :: TrelloAuth -> TrelloP a -> ExceptT TrelloError (Free Triple) a
        g auth = ExceptT . (runExceptT . foldFree (ExceptT . k auth) )

        k :: TrelloAuth -> TrelloF a -> Free Triple (Either TrelloError a)
        k auth c = g1 c >> g2 auth c >> g3 c

        g1 :: (Functor f) => TrelloF a -> Free (DescribeF :+: f) a
        g1 = liftL . t2d

        g2 :: (Functor f, Functor g) => TrelloAuth -> TrelloF a -> Free (f :+: (DescribeF :+: g)) (Either TrelloError a)
        g2 auth = liftR . liftL . runExceptT . t2v auth

        g3 :: (Functor f, Functor g) => TrelloF a -> Free (f :+: (g :+: HttpUriF TrelloAuth)) (Either TrelloError a)
        g3 = liftR . liftR . runExceptT . t2h


type T m a = ExceptT TrelloError (ExceptT HttpError m) a
type Triple = (DescribeF :+: (DescribeF :+: HttpUriF TrelloAuth))


t2d :: TrelloF a -> DescribeP a
t2d = trelloToDescribeI

t2h :: TrelloF a -> ExceptT TrelloError (HttpUriP TrelloAuth) a
t2h = ExceptT . trelloToNetworkI

t2v :: TrelloAuth -> TrelloF a -> ExceptT TrelloError DescribeP a
t2v auth p = mapExceptT (foldFree (h2d auth)) (t2h p)

h2d :: t -> HttpUriF t a -> DescribeP a
h2d = httpUriToDescribe

h2dr :: HttpUriF x a -> R.ReaderT x DescribeP a
h2dr p = do
  x <- R.ask
  R.lift (httpUriToDescribe x p)

h2io :: (MonadIO m, R.MonadReader x m) => HttpUriF x a -> ExceptT HttpError m a
h2io = ExceptT . execHttpUri

d2io :: (MonadIO m) => DescribeF a -> m a
d2io = execDescribe
