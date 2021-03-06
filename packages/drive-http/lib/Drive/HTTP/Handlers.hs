{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Drive.HTTP.Handlers
  ( SupportsNetwork

  , httpToDescribe
  , httpUriToDescribe
  , httpHeaderToDescribe

  , execHttp
  , execHttpUri
  , execHttpHeader
  ) where

import qualified Control.Exception      as E
import qualified Control.Monad.IO.Class as IOC
import qualified Data.ByteString.Lazy   as D
import qualified Data.Text              as Text
import qualified Network.Wreq           as W

import           Control.Lens           ((^.))
import           Control.Monad.Reader
import           Data.Semigroup         ((<>))
import           Drive.Describe
import           Drive.HTTP.Types
import           Network.HTTP.Client    (HttpException)
import           Network.Wreq.Lens


type SupportsNetwork m = (IOC.MonadIO m)


httpToDescribe :: HttpF b -> DescribeP b
httpToDescribe (Get _o u a)
  = a D.empty <$ verbose message
    where
      message = "getting \"" <> Text.pack (u <> "\"")


httpUriToDescribe
  :: forall t b.
     t
  -> HttpUriF t b
  -> DescribeP b

httpUriToDescribe x (GetUri _o u a)
  = a (Left NoContent) <$ verbose (message x)
    where
      message y = "getting \"" <> Text.pack (u y <> "\"")


httpHeaderToDescribe :: t -> HttpHeaderF t b -> DescribeP b
httpHeaderToDescribe x (GetOptions o u a)
  = a D.empty <$ verbose (message x)
    where
      message y
        = "getting \""
        <> Text.pack (u <> "\"")
        <> " with "
        <> Text.pack (show $ o y)


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
  x <- asks u
  (fmap . fmap) (a . Right) (runGet opts x)


execHttpHeader
  :: (IOC.MonadIO m, MonadReader y m)
  => HttpHeaderF y a
  -> m (Either HttpError a)

execHttpHeader (GetOptions o u a) = do
  y <- ask
  let opts = o y
  t <- runGet opts u
  case t of
    Left _ -> undefined
    Right t' -> do
        liftIO $ print t'
        pure (Right $ a t')


runGet :: (MonadIO m) => W.Options -> String -> m (Either HttpError D.ByteString)
runGet opts u
  = IOC.liftIO $ E.catch
      ( do
        res <- W.getWith opts u

        pure $ case res ^. (responseStatus . statusCode) of
          200 -> Right (res ^. responseBody)
          x   -> Left $ RequestError $ show x
      )
      (pure . handle)

  where
    handle :: HttpException -> Either HttpError D.ByteString
    handle e = Left $ RequestError $ show e
