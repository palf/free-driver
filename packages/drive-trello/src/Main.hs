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
    runExceptT (describeVerbose program) >>= liftIO . print
    runExceptT (describeBoth program) >>= liftIO . print
    runSilent program >>= liftIO . print
    runExceptT (runExceptT (runDescribe program)) >>= liftIO . print
    pure ()
  >>= either print (const (print ("done" :: String)))

  where
    -- describe :: (MonadIO m) => TrelloP a -> m a
    describe = t2d >---> d2io


    describeVerbose :: (MonadIO m) => TrelloP a -> ExceptT TrelloError (AuthReader m) a
    describeVerbose
      -- =  (\p' ->  k3 `R.mapReaderT` k2 p') `mapExceptT` k1 p
      =  mapExceptT (R.mapReaderT k3 . k2) . k1

      where
        k1 :: TrelloP a -> ExceptT TrelloError (HttpUriP TrelloAuth) a
        k1 = foldFree t2h
        k2 :: HttpUriP x a -> R.ReaderT x DescribeP a
        k2 = foldFree h2d
        k3 :: (MonadIO m) => DescribeP a -> m a
        k3 = foldFree d2io


    describeBoth :: (MonadIO m) => TrelloP a -> ExceptT TrelloError (AuthReader m) a
    describeBoth = mapExceptT (R.mapReaderT k2) . k1

      where
        k1 = foldFree (ExceptT . g0)
        k2 = foldFree (d2io >---< d2io)

        g0 c = lift (g1 c) >> g2 c
        g1 = liftL . t2d
        g2 = R.mapReaderT liftR . runExceptT . t2v


    runSilent :: (MonadIO m, R.MonadReader TrelloAuth m) => TrelloP a -> m (Either HttpError (Either TrelloError a))
    runSilent
      = ( runExceptT . foldFree h2io ) . ( runExceptT . foldFree t2h )


    -- describe, verbose and run
    runDescribe :: (MonadIO m) => TrelloP a -> ExceptT TrelloError (ExceptT HttpError (AuthReader m)) a
    runDescribe p
      = ExceptT $ R.ask >>= k2 . R.runReaderT (k1 p)

      where
        -- k1 :: TrelloP a -> ExceptT TrelloError (AuthReader (Free Triple)) a
        k1 = runExceptT . foldFree (ExceptT . g0)

        -- k2 :: (MonadIO m) => Free Triple a -> ExceptT HttpError (AuthReader m) a
        k2 = foldFree (d2io >---< (d2io >---< h2io))

        -- g0 :: TrelloF a -> AuthReader (Free Triple) (Either TrelloError a)
        g0 c = R.lift (g1 c) >> g2 c >> R.lift (g3 c)

        -- g1 :: (Functor f) => TrelloF a -> Free (DescribeF :+: f) a
        g1 = liftL . t2d

        -- g2 :: (Functor f, Functor g) => TrelloF a -> AuthReader (Free (f :+: (DescribeF :+: g))) (Either TrelloError a)
        g2 = R.mapReaderT (liftR . liftL) . runExceptT . t2v

        -- g3 :: (Functor f, Functor g) => TrelloF a -> Free (f :+: (g :+: HttpUriF TrelloAuth)) (Either TrelloError a)
        g3 = liftR . liftR . runExceptT . t2h



t2d :: TrelloF a -> DescribeP a
t2d = trelloToDescribeI

t2h :: TrelloF a -> ExceptT TrelloError (HttpUriP TrelloAuth) a
t2h = ExceptT . trelloToNetworkI

t2v :: TrelloF a -> ExceptT TrelloError (AuthReader DescribeP) a
t2v = mapExceptT (foldFree h2d) . t2h

h2d :: HttpUriF x a -> R.ReaderT x DescribeP a
h2d p = do
  x <- R.ask
  R.lift (httpUriToDescribe x p)

h2io :: (MonadIO m, R.MonadReader x m) => HttpUriF x a -> ExceptT HttpError m a
h2io = ExceptT . execHttpUri

d2io :: (MonadIO m) => DescribeF a -> m a
d2io = execDescribe
