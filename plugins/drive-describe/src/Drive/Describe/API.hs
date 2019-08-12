module Drive.Describe.API
  ( debug
  , verbose
  , warn
  ) where

import           Data.Text            (Text)
import           Drive                (Free, liftF)
import           Drive.Describe.Types


mkDescribe :: PriorityLevel -> Text -> Free DescribeF ()
mkDescribe l t = liftF $ LogEntry l t ()


debug :: Text -> Free DescribeF ()
debug = mkDescribe Debug


verbose :: Text -> Free DescribeF ()
verbose = mkDescribe Verbose


warn :: Text -> Free DescribeF ()
warn = mkDescribe Warn
