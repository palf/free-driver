module Test.Drive.Basics03
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Drive.Algebras
import Drive

tests :: TestTree
tests = testGroup "basics 03"
  [ testCase "describe does not run computation" assertDescribeDoesNotRunComputation
  , testCase "simple and complex are interchangeable" assertSimpleAndComplexGiveSameResult
  , testCase "simple and complex are convertable" assertConvertingSimpleToComplexPreservesResult
  -- , simpleToDescribe
  -- , warToDescribe
  -- , simpleToWarToDescribe
  -- , leftSimpleWarRightSimpleWarDescribe
  -- , leftSimpleWarDescribeRightSimpleWar
  ]


runSimple :: Int -> SimpleM Int a -> IO a
runSimple = runSimpleM

runComplex :: Int -> ComplexM Int a -> IO a
runComplex = runComplexM

runDescribe :: DescribeM a -> IO a
runDescribe = runDescribeM

-- asComplex :: (Convertable f ( ComplexF t )) => Free f a -> ComplexM t a
-- asComplex = foldFree convert

asComplex :: SimpleM Int a -> ComplexM Int a
asComplex = foldFree convert

asDescribe :: (Convertable f DescribeF) => Free f a -> DescribeM a
asDescribe = foldFree convert


assertDescribeDoesNotRunComputation :: IO ()
assertDescribeDoesNotRunComputation = do
  x <- runDescribe describeP
  y <- runDescribe (asDescribe joinedAlg)
  x @=? y

    where

      describeP :: DescribeM Int
      describeP = pure 0 <$> entry "zero"

      joinedAlg :: Free (SimpleF Int :+: ComplexF Int) Int
      joinedAlg = do
        x <- liftL   double
        y <- liftR $ multiply 3
        pure $ x + y


assertSimpleAndComplexGiveSameResult :: IO ()
assertSimpleAndComplexGiveSameResult = do
  x <- runSimple 4 triple
  y <- runComplex 4 (multiply 3)
  x @=? y

assertConvertingSimpleToComplexPreservesResult :: IO ()
assertConvertingSimpleToComplexPreservesResult = do
  x <- runComplex 5 (asComplex triple)
  y <- runComplex 5 (multiply 3)
  x @=? y
