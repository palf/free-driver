{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module NetworkSession
  ( NetworkSessionF
  , SupportsNetworkSession
  , inSession
  , get
  , execNetworkSession
  ) where

import           Control.Lens           ((^.))
import qualified Control.Monad.Free     as Free
import qualified Control.Monad.IO.Class as IOC
import qualified Control.Monad.Reader   as R
import qualified Data.ByteString.Lazy   as D
import           Data.Monoid            ((<>))
import qualified Data.Text              as Text
import           Drive                  (type (-<))
import qualified Drive                  as D
import qualified Network.Wreq           as W
import qualified Network.Wreq.Session   as S


type Uri = String


data NetworkSessionF a
  = Get W.Options Uri (D.ByteString -> a)
  deriving (Functor)


get :: W.Options -> String -> Free NetworkSessionF D.ByteString
get opts u = Free.liftF $ Get opts u id





networkSessionToDescribeI :: (NetworkSessionF -< D.DescribeF) a
networkSessionToDescribeI (Get _ u a) = logGetUri u >> pure (a "from get")


networkSessionToNetworkI :: (NetworkSessionF -< D.HTTPF) a
networkSessionToNetworkI (Get o u a) = D.get o u >> pure (a "from get")


logGetUri :: String -> Free D.DescribeF ()
logGetUri u
  = D.verbose ("get (" <> Text.pack u <> ")")


type SupportsNetworkSession m = (R.MonadReader S.Session m, IOC.MonadIO m)


inSession :: R.ReaderT S.Session IO a -> IO a
inSession = S.withSession . R.runReaderT



execNetworkSession :: (SupportsNetworkSession m) => NetworkSessionF a -> m a
execNetworkSession (Get opts u a) = do
  s <- R.ask
  lbz <- IOC.liftIO (S.getWith opts s u)
  let x = lbz ^. W.responseBody
  pure $ a x
