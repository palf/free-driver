{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Drive
  ( module X
  , F.Free
  , F.liftF
  , F.foldFree
  , foldEitherFree
  , foldMaybeFree
  ) where

import qualified Control.Monad.Free as F
import           Drive.Interpreter  as X


newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  -- fmap f (EitherT m) = EitherT $ (fmap f) <$> m
  fmap f (EitherT m) = EitherT $  (fmap . fmap) f m

instance Monad m => Applicative (EitherT e m) where
  pure = EitherT . pure . Right
  EitherT f <*> EitherT v =
    EitherT $ f >>= either
      (pure . Left)
      -- (\k -> (fmap . fmap) k v)
      (\k -> (fmap k) <$> v)


instance Monad m => Monad (EitherT e m) where
  return = pure
  EitherT x >>= f =
    EitherT $ x >>= either
      (pure . Left)
      (runEitherT . f)


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
foldMaybeFree f (F.Free p) = f p >>= \case
    Nothing ->  pure Nothing
    Just v -> foldMaybeFree f v
