{-# LANGUAGE RankNTypes #-}

module Drive
  ( module X
  , F.Free
  , F.liftF
  , F.foldFree
  , foldEitherFree
  , foldMaybeFree
  ) where

import Drive.Interpreter as X
import qualified Control.Monad.Free as F


newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT m) = EitherT $ fmap (fmap f) m

instance Monad m => Applicative (EitherT e m) where
  pure = EitherT . pure . Right
  EitherT f <*> EitherT v
    = EitherT $ f
      >>= \mf -> case mf of
        Left e  -> pure (Left e)
        Right k -> v
          >>= \mv -> case mv of
            Left e  -> pure (Left e)
            Right x -> pure (Right (k x))


instance Monad m => Monad (EitherT e m) where
  return = pure
  EitherT x >>= f = EitherT $ do
    res <- x
    case res of
      Left e -> pure (Left e)
      Right v -> runEitherT . f $ v


foldEitherFree
  :: Monad m
  => (forall x. t x -> m (Either e x))
  -> F.Free t a
  -> m (Either e a)

foldEitherFree f = runEitherT . F.foldFree (EitherT . f)

--foldEitherFree _ (Pure v) = pure (Right v)
--foldEitherFree f (Free p) = f p >>= \x ->
--  case x of
--    Left e ->  pure (Left e)
--    Right v -> foldEitherFree f v


foldMaybeFree
  :: Monad f
  => (forall x. t x -> f (Maybe x))
  -> F.Free t b
  -> f (Maybe b)

foldMaybeFree _ (F.Pure v) = pure (Just v)
foldMaybeFree f (F.Free p) = f p >>= \x ->
  case x of
    Nothing ->  pure Nothing
    Just v -> foldMaybeFree f v
