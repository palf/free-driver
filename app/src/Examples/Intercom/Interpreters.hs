{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.Intercom.Interpreters
   where

import qualified Control.Monad.Reader as R
import qualified Drive                as D
import qualified Drive.Describe       as D
import qualified Drive.HTTP           as H
import qualified Drive.Intercom       as I

import           Control.Monad        (void)
import           Control.Monad.Free   (Free (..))


type DescribeP     = D.Free D.DescribeF
type HttpIntercomP = D.Free (H.HttpHeaderF I.IntercomCredentials)
type IntercomP     = D.Free I.IntercomF
type HError        = H.HttpError
type IError        = I.IntercomError


ff :: Monad m => (forall x. f x -> m x) -> D.Free f a -> m a
ff = D.foldFree


fe :: Monad m => (forall x. t x -> m (Either e x)) -> Free t a -> m (Either e a)
fe = D.foldEitherFree


asDebug :: IntercomP a -> DescribeP a
asDebug = ff I.intercomToDescribeI


asHttp :: IntercomP a -> HttpIntercomP (Either IError a)
asHttp = fe I.intercomToNetworkI


httpAsDescribe :: I.IntercomCredentials -> HttpIntercomP a -> DescribeP a
httpAsDescribe auth = ff (H.httpHeaderToDescribe auth)


asVerbose
  :: IntercomP a
  -> R.ReaderT I.IntercomCredentials DescribeP (Either IError a)

asVerbose p
  = R.ask >>= \auth -> R.lift $ httpAsDescribe auth $ asHttp p


runDescribe :: (R.MonadIO m) => DescribeP a -> m ()
runDescribe = void . ff D.execDescribe


runDescribeR :: (R.MonadIO m, R.MonadReader r m) => R.ReaderT r DescribeP a -> m ()
runDescribeR p = R.ask >>= void . ff D.execDescribe . R.runReaderT p


runHttp
  :: (R.MonadIO m, R.MonadReader I.IntercomCredentials m)
  => HttpIntercomP a
  -> m (Either HError a)

runHttp = fe H.execHttpHeader
