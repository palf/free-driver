{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators     #-}

module Main
  ( main
  ) where

import Control.Monad.Reader as R
import Control.Monad.Writer as W

import Control.Monad.Trans.Maybe
import Control.Monad.Free
import Data.Functor
import           Data.Text              (Text, pack)
import           Control.Monad.Free.TH


data HighF a
  = H1 a
  | H2 a
  deriving (Functor)

data MidF a
  = M1 a
  | M2 a
  | M3 (Int -> a)
  | M4 (Text -> a)
  deriving (Functor)

data LowF a
  = L1 a
  | L2 a
  | L3 a
  deriving (Functor)


makeFree ''HighF
makeFree ''MidF
makeFree ''LowF

type Program = Free HighF


program :: Program ()
program = h1 >> h2 $> ()


main :: IO ()
main = do
  execWriterT (describe1 program) >>= print
  execWriterT (runReaderT (describe2 program) (5 ,"fish" )) >>= print
  execWriterT (runReaderT (describe3 program) 6) >>= print
  execWriterT (describe4 program) >>= print

  where
    describe1 :: (Monad m, MonadWriter [Text] m) => Program a -> m a
    describe1 = k1

    describe4 :: (Monad m, MonadWriter [Text] m) => Program a -> m a
    describe4 = k6 . k2

    describe2 :: (Monad m, MonadWriter [Text] m) => Program a -> R.ReaderT (Int, Text) m a
    describe2 = combineReaders . k4 . k3

    describe3 :: (Monad m, MonadWriter [Text] m) => Program a -> R.ReaderT Int m (Maybe a)
    describe3 = mapReaderT k6 . k5 . k3


combineReaders :: R.ReaderT a (R.ReaderT b m) x -> R.ReaderT (a, b) m x
combineReaders r = ReaderT $ \(a, b) -> runReaderT (runReaderT r a) b


h2text :: (Monad m, MonadWriter [Text] m) => HighF a -> m a
h2text (H1 a) = tell ["h1"] $> a
h2text (H2 a) = tell ["h2"] $> a

m2text :: (Monad m, MonadWriter [Text] m) => MidF a -> R.ReaderT Int (R.ReaderT Text m) a
m2text (M1 a) = tell ["m1"] $> a
m2text (M2 a) = tell ["m2"] $> a
m2text (M3 f) = do
  tell ["m3"]
  x <- ask
  tell [pack $ show x]
  pure $ f x
m2text (M4 f) = do
  tell ["m4"]
  x <- lift ask
  tell [pack $ show x]
  pure $ f x

l2text :: (Monad m, MonadWriter [Text] m) => LowF a -> m a
l2text (L1 a) = tell ["l1"] $> a
l2text (L2 a) = tell ["l2"] $> a
l2text (L3 a) = tell ["l3"] $> a


h2m :: HighF a -> Free MidF a
h2m (H1 a) = (m1 >> m3) $> a
h2m (H2 a) = (m1 >> m2 >> m4) $> a

h2l :: HighF a -> Free LowF a
h2l (H1 a) = (l1 >> l2 >> l3) $> a
h2l (H2 a) = (l1 >> l2 >> l3) $> a

m2l :: (MonadFree LowF m, MonadReader Int m) => MidF a -> m (Maybe a)
m2l (M1 a) = l1 >> l2 $> Just a
m2l (M2 a) = l1 >> l3 $> Just a
m2l (M3 f) = l1 >> l3 >> asks (Just . f)
m2l (M4 _) = l3 $> Nothing


k1 :: (MonadWriter [Text] m) => Free HighF a -> m a
k1 = foldFree h2text
k2 :: () => Free HighF a -> Free LowF a
k2 = foldFree h2l
k3 :: () => Free HighF a -> Free MidF a
k3 = foldFree h2m
k4 :: (MonadWriter [Text] m) => Free MidF a -> R.ReaderT Int (R.ReaderT Text m) a
k4 = foldFree m2text
k5 :: (MonadFree LowF m, MonadReader Int m) => Free MidF a -> m (Maybe a)
k5 = runMaybeT . foldFree (MaybeT . m2l)
k6 :: (MonadWriter [Text] m) => Free LowF a -> m a
k6 = foldFree l2text

