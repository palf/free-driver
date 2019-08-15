{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Drive.Describe.Handlers
  ( execDescribe
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Functor           (($>))
import           Data.Semigroup         ((<>))
import           Data.Text              (Text, unpack)
import           Drive.Describe.Types


execDescribe :: (MonadIO m) => DescribeF a -> m a
execDescribe (LogEntry l t a) = liftIO $ printMessage l t $> a


printMessage :: PriorityLevel -> Text -> IO ()
printMessage Debug t   = putStrLn ("debug   :\t" <> unpack t)
printMessage Verbose t = putStrLn ("verbose :\t" <> unpack t)
printMessage Warn t    = putStrLn ("WARN    :\t" <> unpack t)
