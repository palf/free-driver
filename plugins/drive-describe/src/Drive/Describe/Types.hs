{-# LANGUAGE DeriveFunctor #-}

module Drive.Describe.Types
  ( DescribeF (..)
  , PriorityLevel (..)
  ) where

import qualified Data.Text as T


data PriorityLevel
  = Debug
  | Verbose
  | Warn


data DescribeF a = LogEntry PriorityLevel T.Text a
  deriving (Functor)
