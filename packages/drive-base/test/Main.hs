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

import Control.Monad.Free
import Data.Functor
import           Data.Text              (Text, pack)
import           Control.Monad.Free.TH


data HF a
  = H1 a
  | H2 a
  deriving (Functor)

data MF a
  = M1 a
  | M2 a
  | M3 (Int -> a)
  | M4 (Text -> a)
  deriving (Functor)

data LF a
  = L1 a
  | L2 a
  | L3 a
  deriving (Functor)


makeFree ''HF
makeFree ''MF
makeFree ''LF


program :: Free HF ()
program = do
  h1
  h2
  pure ()


h2text :: (Monad m, MonadWriter [Text] m) => HF a -> m a
h2text (H1 a) = tell ["h1"] $> a
h2text (H2 a) = tell ["h2"] $> a

m2text :: (Monad m, MonadWriter [Text] m) => MF a -> R.ReaderT Int (R.ReaderT Text m) a
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

l2text :: (Monad m, MonadWriter [Text] m) => LF a -> m a
l2text (L1 a) = tell ["l1"] $> a
l2text (L2 a) = tell ["l2"] $> a
l2text (L3 a) = tell ["l3"] $> a


h2m :: HF a -> Free MF a
h2m (H1 a) = (m1 >> m2) $> a
h2m (H2 a) = (m2 >> m3) $> a

h2l :: HF a -> Free LF a
h2l (H1 a) = (l1 >> l2 >> l3) $> a
h2l (H2 a) = (l2 >> l3 >> l1) $> a

m2l :: (MonadFree LF m, MonadReader Int m) => MF a -> m a
m2l (M1 a) = (l1 >> l2) $> a
m2l (M2 a) = (l2 >> l3) $> a
m2l (M3 f) = do
  l3
  l1
  asks f



main :: IO ()
main = do
  print (execWriter $ describe program)
  runReaderT (execWriterT (describe2 program)) 5 >>= print
  execWriterT ( runReaderT (describe3 program)) 6 >>= print
  print (execWriter $ describe4 program)

  where
    describe :: (Monad m, MonadWriter [Text] m) => Free HF a -> m a
    describe = k4

    describe2 :: (Monad m, MonadReader Int m, MonadWriter [Text] m) => Free HF a -> m a
    describe2 = k5 . k1

    describe3 :: (Monad m, MonadWriter [Text] m) => Free HF a -> R.ReaderT Int m a
    describe3 = mapReaderT k6 . k3 . k1

    describe4 :: (Monad m) => Free HF a -> WriterT [Text] m a
    describe4 = k6 . k2


k1 :: () => Free HF a -> Free MF a
k1 = foldFree h2m
k2 :: () => Free HF a -> Free LF a
k2 = foldFree h2l
k3 :: (MonadFree LF m, MonadReader Int m) => Free MF a -> m a
k3 = foldFree m2l
k4 :: (MonadWriter [Text] m) => Free HF a -> m a
k4 = foldFree h2text
k5 :: (MonadReader Int m, MonadWriter [Text] m) => Free MF a -> m a
k5 = _ foldFree m2text
k6 :: (MonadWriter [Text] m) => Free LF a -> m a
k6 = foldFree l2text
