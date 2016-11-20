module Test.Drive.Algebras
  where

import Drive
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.IO.Class
import Data.Text
import Data.Monoid


data SimpleF t a
  = Double (t -> a)
  | Triple (t -> a)
  deriving (Functor)


data ComplexF t a
  = Multiply t (t -> a)
  deriving (Functor)


data DescribeF a
  = Entry Text a
  deriving (Functor)


makeFree ''SimpleF
makeFree ''ComplexF
makeFree ''DescribeF

type SimpleM t a  = Free (SimpleF t) a
type ComplexM t a = Free (ComplexF t) a
type DescribeM a  = Free DescribeF a

runSimpleF :: (Monad m, Num t) => t -> SimpleF t a -> m a
runSimpleF x (Double f) = f <$> pure (x + x)
runSimpleF x (Triple f) = pure (x + x + x) >>= pure . f

runSimpleM :: (Monad m, Num t) => t -> SimpleM t a -> m a
runSimpleM n = foldFree (runSimpleF n)

runComplexF :: (Monad m, Num t) => t -> ComplexF t a -> m a
runComplexF x (Multiply n f) = pure (n * x) >>= pure . f

runComplexM :: (Monad m, Num t) => t -> ComplexM t a -> m a
runComplexM x = foldFree (runComplexF x)

runDescribeF :: (MonadIO m) => DescribeF a -> m a
runDescribeF (Entry t a) = printText t $> a
  where printText = liftIO . print . unpack

runDescribeM :: (MonadIO m) => DescribeM a -> m a
runDescribeM = foldFree runDescribeF



class Convertable f g where
  convert :: f a -> Free g a

instance (Convertable f x, Convertable g x) => Convertable (f :+: g) x where
  convert (D f) = convert f
  convert (R g) = convert g

instance (Num t) => Convertable (SimpleF t) DescribeF where
  convert (Double f) = f 0 <$ entry "double"
  convert (Triple f) = f 0 <$ entry "triple"

instance (Num t) => Convertable (SimpleF t) (ComplexF t) where
  convert (Double f) = f <$> multiply 2
  convert (Triple f) = f <$> multiply 3

instance (Num t, Show t) => Convertable (ComplexF t) DescribeF where
  convert (Multiply n f) = f 0 <$ entry ("multiply: " <> pack (show n))
