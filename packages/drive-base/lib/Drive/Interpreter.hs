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
  , Combo (..)
  , (:+:)

  , identityI
  , liftL
  , liftR

  , copyI
  , (<--->)

  , chainI
  , (>--->)

  , combineI
  , (>---<)
  ) where

import           Control.Monad.Free (Free)
import qualified Control.Monad.Free as Free

type (:+:) = Combo

data Combo f g a
  = L (f a) | R (g a)
  deriving (Functor)


type NatTran f g a
  = f a -> g a


type Interpreter f g a
  = f a -> Free g a


infixl 9 <--->
(<--->) :: (Functor f, Functor g) => (t a -> Free f a) -> (t a -> Free g a) -> (t a -> Free (f :+: g) a)
(<--->) = copyI


infixl 9 >--->
(>--->) :: (Functor f, Monad g) => (forall y. (p y -> Free f y)) -> (forall x. (f x -> g x)) -> (Free p b -> g b)
(>--->) a b = chainI b a


infixl 9 >---<
(>---<) :: (f a -> m a) -> (g a -> m a) -> ( f :+: g ) a -> m a
(>---<) = combineI


identityI :: (Functor f) => Interpreter f f a
identityI = Free.liftF


liftL :: (Functor f, Functor g) => Free f a -> Free (f :+: g) a
liftL = Free.hoistFree L


liftR :: (Functor f, Functor g) => Free g a -> Free (f :+: g) a
liftR = Free.hoistFree R


copyI
  :: (Functor f, Functor g)
  => Interpreter t f a
  -> Interpreter t g a
  -> Interpreter t (f :+: g) a

copyI a b c
  = liftL (a c) >> liftR (b c)


chainI
  :: (Functor g, Monad h)
  => (forall x. NatTran g h x)
  -> (forall y. Interpreter f g y)
  -> (Free f a -> h a)

chainI a b
  = Free.foldFree a . Free.foldFree b


combineI
  :: NatTran f m a
  -> NatTran g m a
  -> ( f :+: g ) a
  -> m a

combineI f _ (L t) = f t
combineI _ f (R t) = f t
