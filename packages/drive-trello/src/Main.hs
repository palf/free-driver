{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Reader   as R
import qualified Data.Yaml              as Y

import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import           Drive
import           Drive.Describe
import           Drive.HTTP
import           Drive.Trello


type AuthReader = R.ReaderT TrelloAuth


program :: TrelloP [Text]
program
  = fmap boardName <$> getBoards (User "jackpalfrey3")


withTrelloCredentials :: FilePath -> AuthReader IO a -> IO (Either Y.ParseException a)
withTrelloCredentials p f
  = Y.decodeFileEither p
  >>= either (pure . Left) (fmap Right . R.runReaderT f)


main :: IO ()
main = do
  describe program >>= print

  withTrelloCredentials "credentials/trello.yaml" $ do
    describeVerbose program >>= liftIO . print
    describeBoth program >>= liftIO . print
    runSilent program >>= liftIO . print
    runDescribe program >>= liftIO . print
    pure ()
  >>= either print (const (print ("done" :: String)))

  where
    describe :: (MonadIO m) => TrelloP a -> m a
    describe = t2d >---> d2io


    describeVerbose :: (MonadIO m) => TrelloP a -> AuthReader m (Either TrelloError a)
    describeVerbose
      = R.mapReaderT k3 . k2 . k1

      where
        k1 :: TrelloP a -> HttpUriP TrelloAuth (Either TrelloError a)
        k1 = runExceptT . foldFree t2h
        k2 :: HttpUriP x a -> R.ReaderT x DescribeP a
        k2 = foldFree h2d
        k3 :: (MonadIO m) => DescribeP a -> m a
        k3 = foldFree d2io


    describeBoth :: (MonadIO m) => TrelloP a -> AuthReader m (Either TrelloError a)
    describeBoth = R.mapReaderT k2 . k1

      where
        k1 :: TrelloP a -> AuthReader (Free (DescribeF :+: DescribeF)) (Either TrelloError a)
        k1 = runExceptT . foldFree (ExceptT . g0)
        k2 :: (MonadIO m) => Free (DescribeF :+: DescribeF) a -> m a
        k2 = foldFree (d2io >---< d2io)

        g0 :: TrelloF a -> AuthReader (Free (DescribeF :+: DescribeF)) (Either TrelloError a)
        g0 c = lift (g1 c) >> g2 c
        g1 :: (Functor f) => TrelloF a -> Free (DescribeF :+: f) a
        g1 = liftL . t2d
        g2 :: (Functor f) => TrelloF a -> AuthReader (Free (f :+: DescribeF)) (Either TrelloError a)
        g2 = R.mapReaderT liftR . t2v


    runSilent :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> m (Either HttpError (Either TrelloError a))
    runSilent
      = ( runExceptT . foldFree h2io ) . ( runExceptT . foldFree t2h )


    -- describe, verbose and run
    runDescribe :: (MonadIO m) => TrelloP a -> AuthReader m (Either HttpError (Either TrelloError a))
    runDescribe
      = joinReader . R.mapReaderT k2 . k1


      where
        joinReader :: (Monad n) => R.ReaderT r ( R.ReaderT r n ) b -> R.ReaderT r n b
        joinReader r = R.ask >>= R.runReaderT r

        k1 :: TrelloP a -> AuthReader TripleP (Either TrelloError a)
        k1 = runExceptT . foldFree (ExceptT . g0)
        k2 :: (MonadIO m) => TripleP a -> AuthReader m (Either HttpError a)
        k2 = runExceptT . foldFree (d2io >---< (d2io >---< h2io))

        g0 :: TrelloF a -> AuthReader TripleP (Either TrelloError a)
        g0 c = R.lift (g1 c) >> g2 c >> R.lift (g3 c)
        g1 :: (Functor f) => TrelloF a -> Free (DescribeF :+: f) a
        g1 = liftL . t2d
        g2 :: (Functor f, Functor g) => TrelloF a -> AuthReader (Free (f :+: (DescribeF :+: g))) (Either TrelloError a)
        g2 = R.mapReaderT (liftR . liftL) . t2v
        g3 :: (Functor f, Functor g) => TrelloF a -> Free (f :+: (g :+: HttpUriF TrelloAuth)) (Either TrelloError a)
        g3 = liftR . liftR . runExceptT . t2h


type TripleP = Free (DescribeF :+: (DescribeF :+: HttpUriF TrelloAuth))



t2d :: TrelloF a -> DescribeP a
t2d = trelloToDescribeI

t2h :: TrelloF a -> ExceptT TrelloError (HttpUriP TrelloAuth) a
t2h = ExceptT . trelloToNetworkI

t2v :: TrelloF a -> AuthReader DescribeP (Either TrelloError a)
t2v = foldFree h2d . trelloToNetworkI

h2d :: HttpUriF x a -> R.ReaderT x DescribeP a
h2d p = R.ask >>= \x -> R.lift (httpUriToDescribe x p)

h2io :: (MonadIO m, R.MonadReader x m) => HttpUriF x a -> ExceptT HttpError m a
h2io = ExceptT . execHttpUri

d2io :: (MonadIO m) => DescribeF a -> m a
d2io = execDescribe
