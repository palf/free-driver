{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Drive.Interpreter
  ( Interpreter
  , type (-<)
  , (:+:) (..)
  , identityI
  , liftL
  , liftR
  , sumI
  , composeI
  , bimapI
  ) where

import           Control.Monad.Free (Free)
import qualified Control.Monad.Free as Free


data (f :+: g) a
  = D (f a) | R (g a)
  deriving Functor


type Interpreter f g a
  = f a -> Free g a


type (f -< g) a = Interpreter f g a


identityI :: (Functor f) => Interpreter f f a
identityI = Free.liftF


liftL :: (Functor f, Functor g) => Free f a -> Free (f :+: g) a
liftL = Free.hoistFree D


liftR :: (Functor f, Functor g) => Free g a -> Free (f :+: g) a
liftR = Free.hoistFree R


sumI
  :: (Functor f, Functor g)
  => Interpreter t f a
  -> Interpreter t g a
  -> Interpreter t (f :+: g) a

sumI a b c
  = liftL (a c) >> liftR (b c)


composeI
  :: (Functor h)
  => (forall a. Interpreter g h a)
  -> Interpreter f g b
  -> Interpreter f h b

composeI a b
  = Free.foldFree a . b


bimapI
  :: (f a -> m a)
  -> (g a -> m a)
  -> ( f :+: g ) a
  -> m a

bimapI f _ (D t) = f t
bimapI _ f (R t) = f t
