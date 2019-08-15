{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}

module Drive.NetworkSession
  ( NetworkSessionF
  , SupportsNetworkSession
  , inSession
  , sessionGet
  , execNetworkSession
  ) where

import           Control.Lens           ((^.))
import qualified Control.Monad.Free     as Free
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Reader   as R
import qualified Data.ByteString.Lazy   as BS
-- import           Data.Monoid            ((<>))
-- import qualified Data.Text              as Text
import qualified Drive                  as V
import qualified Network.Wreq           as Wreq
import qualified Network.Wreq.Session   as S
-- import Drive.Describe
-- import Drive.HTTP


type Uri = String


data NetworkSessionF a
  = SessionGet Wreq.Options Uri (BS.ByteString -> a)
  deriving (Functor)

type NetworkSessionP = V.Free NetworkSessionF

type SupportsNetworkSession m = (R.MonadReader S.Session m, MonadIO m)


sessionGet :: Wreq.Options -> String -> NetworkSessionP BS.ByteString
sessionGet opts u = Free.liftF $ SessionGet opts u id


-- networkSessionToDescribeI :: (V.Interpreter NetworkSessionF DescribeF) a
-- networkSessionToDescribeI (SessionGet _ u a) = logGetUri u >> pure (a "from get")


-- networkSessionToHttpI :: (V.Interpreter NetworkSessionF HttpF) a
-- networkSessionToHttpI (SessionGet o u a) = Get o u >> pure (a "from sessionGet")


-- logGetUri :: String -> DescribeP ()
-- logGetUri u
--   = verbose ("sessionGet (" <> Text.pack u <> ")")


inSession :: R.ReaderT S.Session IO a -> IO a
inSession p = S.newSession >>= R.runReaderT p


execNetworkSession :: (SupportsNetworkSession m) => NetworkSessionF a -> m a
execNetworkSession (SessionGet opts u a) = do
  s <- R.ask
  lbz <- liftIO (S.getWith opts s u)
  let x = lbz ^. Wreq.responseBody
  pure $ a x
