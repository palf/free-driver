{-# LANGUAGE DeriveFunctor #-}

module Drive.Describe.Types
  ( DescribeF (..)
  , PriorityLevel (..)
  ) where

import           Data.Text (Text)


data PriorityLevel
  = Debug
  | Verbose
  | Warn


data DescribeF a = LogEntry PriorityLevel Text a
  deriving (Functor)
