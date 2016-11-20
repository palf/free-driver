{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Drive.HTTP.Handlers
  ( SupportsNetwork
  , execHttp
  , execHttpUri
  , execHttpHeader
  , httpToLog
  , httpUriToDescribe
  , httpHeaderToLog
  ) where

import           Control.Monad.Reader
import           Drive
import           Drive.HTTP.Types
import           Drive.Describe
import           Network.Wreq.Lens
import qualified Control.Exception      as E
import qualified Control.Monad.IO.Class as IOC
import qualified Data.ByteString.Lazy   as D
import qualified Data.Text              as T
import qualified Network.Wreq           as W

import           Control.Lens        ((^.))
import           Data.Semigroup      ((<>))
import           Network.HTTP.Client (HttpException)

type SupportsNetwork m = (IOC.MonadIO m)


httpToLog :: HttpF b -> Free DescribeF b
httpToLog (Get _o u a)
  = a D.empty <$ verbose message
    where
      message = "getting \"" <> T.pack (u <> "\"")


httpUriToDescribe
  :: forall t b.
     t
  -> HttpUriF t b
  -> Free DescribeF b

httpUriToDescribe x (GetUri _o u a)
  = a (Left NoContent) <$ verbose (message x)
    where
      message y = "getting \"" <> T.pack (u y <> "\"")

httpHeaderToLog :: t -> HttpHeaderF t b -> Free DescribeF b
httpHeaderToLog x (GetOptions o u a)
  = a D.empty <$ verbose (message x)
    where
      message y
        = "getting \""
        <> T.pack (u <> "\"")
        <> " with "
        <> T.pack (show $ o y)


execHttp :: (IOC.MonadIO m) => HttpF a -> m a
execHttp (Get opts u a) = do
  lbz <- IOC.liftIO (W.getWith opts u)
  let x = lbz ^. W.responseBody
  pure $ a x


execHttpUri
  :: (IOC.MonadIO m, MonadReader x m)
  => forall a.
     HttpUriF x a
  -> m (Either HttpError a)

execHttpUri (GetUri opts u a) = do
  x <- ask
  let u' = u x
  (fmap . fmap) (a . Right) (runGet opts u')


execHttpHeader :: (IOC.MonadIO m, MonadReader y m) => HttpHeaderF y a -> m a
execHttpHeader (GetOptions o u a) = do
  y <- ask
  let opts = o y
  t <- runGet opts u
  case t of
    Left _ -> undefined
    Right t' -> do
        liftIO $ print t'
        pure (a t')


runGet :: (MonadIO m) => W.Options -> String -> m (Either HttpError D.ByteString)
runGet opts u
  = IOC.liftIO $ E.catch
      ( do
        res <- W.getWith opts u

        case res ^. responseStatus ^. statusCode of
          200 -> do
            let x = res ^. responseBody
            pure $ Right x

          _ -> pure (Left RequestError)
      )
      handle

  where
    handle :: (MonadIO m) => HttpException -> m (Either HttpError D.ByteString)
    handle _e = pure (Left RequestError)
