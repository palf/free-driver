{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Drive.Describe.Handlers
  ( SupportsLog
  , execDescribe
  ) where


import           Drive.Describe.Types

import qualified Control.Monad.IO.Class as IOC
import qualified Data.Text              as T

import           Data.Functor           (($>))
import           Data.Semigroup         ((<>))


type SupportsLog m = (IOC.MonadIO m)


execDescribe :: (SupportsLog m) => DescribeF a -> m a
execDescribe (LogEntry l t a) = IOC.liftIO $ printMessage l t $> a


printMessage :: PriorityLevel -> T.Text -> IO ()
printMessage Debug t   = putStrLn ("debug   :\t" <> T.unpack t)
printMessage Verbose t = putStrLn ("verbose :\t" <> T.unpack t)
printMessage Warn t    = putStrLn ("WARN    :\t" <> T.unpack t)
