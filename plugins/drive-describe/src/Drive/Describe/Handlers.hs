{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Drive.Describe.Handlers
  ( SupportsLog
  , execDescribe
  ) where


import qualified Control.Monad.IO.Class as IOC
import           Data.Functor           (($>))
import           Data.Semigroup         ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Drive.Describe.Types


type SupportsLog m = (IOC.MonadIO m)


execDescribe :: (SupportsLog m) => DescribeF a -> m a
execDescribe (LogEntry l t a) = IOC.liftIO $ printMessage l t $> a


printMessage :: PriorityLevel -> Text -> IO ()
printMessage Debug t   = putStrLn ("debug   :\t" <> Text.unpack t)
printMessage Verbose t = putStrLn ("verbose :\t" <> Text.unpack t)
printMessage Warn t    = putStrLn ("WARN    :\t" <> Text.unpack t)
