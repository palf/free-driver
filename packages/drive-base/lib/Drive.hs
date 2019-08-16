{-# LANGUAGE RankNTypes #-}

module Drive
  ( module X
  , Free
  , Free.liftF
  , Free.foldFree
  , foldEitherFree
  , foldEitherEitherFree
  , foldMaybeFree
  ) where

import           Control.Monad.Free (Free (..))
import qualified Control.Monad.Free as Free
import           Drive.Interpreter  as X
import Control.Monad.Trans.Maybe
import Control.Monad.Except


-- newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}

-- instance Functor m => Functor (ExceptT e m) where
--   fmap f (ExceptT m) = ExceptT $ (fmap . fmap) f m

-- instance Monad m => Applicative (ExceptT e m) where
--   pure = ExceptT . pure . Right
--   ExceptT f <*> ExceptT v =
--     ExceptT $ f >>= either
--       (pure . Left)
--       (\k -> fmap k <$> v)


-- instance Monad m => Monad (ExceptT e m) where
--   return = pure
--   ExceptT x >>= f =
--     ExceptT $ x >>= either
--       (pure . Left)
--       (runExceptT . f)


foldEitherFree
  :: Monad m
  => (forall x. t x -> m (Either e x))
  -> Free t a
  -> m (Either e a)

foldEitherFree f = runExceptT . Free.foldFree (ExceptT . f)

--foldEitherFree _ (Pure v) = pure (Right v)
--foldEitherFree f (Free p) = f p >>= \x ->
--  case x of
--    Left e ->  pure (Left e)
--    Right v -> foldEitherFree f v


foldEitherEitherFree
  :: Monad m
  => (forall x. t x -> m (Either e1 (Either e2 x)))
  -> Free t a
  -> m (Either e1 (Either e2 a))

foldEitherEitherFree _ (Pure v) = pure (Right $ Right v)
foldEitherEitherFree f (Free p) =
  f p >>=
    either
      (pure . Left)
      (either
        (pure . Right . Left)
        (foldEitherEitherFree f)
      )


foldMaybeFree
  :: Monad f
  => (forall x. t x -> f (Maybe x))
  -> Free t b
  -> f (Maybe b)

foldMaybeFree f = runMaybeT . Free.foldFree (MaybeT . f)

-- foldMaybeFree _ (Pure v) = pure (Just v)
-- foldMaybeFree f (Free p) = f p >>= \case
--     Nothing ->  pure Nothing
--     Just v -> foldMaybeFree f v
