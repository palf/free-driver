module Drive.Describe.API
  ( DescribeP
  , debug
  , verbose
  , warn
  ) where

import           Data.Text            (Text)
import           Drive                (Free, liftF)
import           Drive.Describe.Types


type DescribeP  = Free DescribeF


mkDescribe :: PriorityLevel -> Text -> DescribeP ()
mkDescribe l t = liftF $ LogEntry l t ()


debug :: Text -> DescribeP ()
debug = mkDescribe Debug


verbose :: Text -> DescribeP ()
verbose = mkDescribe Verbose


warn :: Text -> DescribeP ()
warn = mkDescribe Warn
