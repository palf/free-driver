module Drive.Describe.API
  ( debug
  , verbose
  , warn
  ) where

import           Drive.Describe.Types

import qualified Data.Text              as T

import           Drive          (Free, liftF)


mkDescribe :: PriorityLevel -> T.Text -> Free DescribeF ()
mkDescribe l t = liftF $ LogEntry l t ()


debug :: T.Text -> Free DescribeF ()
debug = mkDescribe Debug


verbose :: T.Text -> Free DescribeF ()
verbose = mkDescribe Verbose


warn :: T.Text -> Free DescribeF ()
warn = mkDescribe Warn
