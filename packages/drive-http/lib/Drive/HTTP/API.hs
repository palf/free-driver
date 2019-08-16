{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Drive.HTTP.API
  ( HttpP
  , HttpUriP
  , HttpHeaderP
  , W.defaults
  , get
  , getRaw
  , getRawUrl
  , getRawOpts
  ) where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import qualified Data.Aeson            as Aeson
import qualified Data.ByteString.Lazy  as D
import           Drive                 (Free, liftF)
import           Drive.HTTP.Types
import qualified Network.Wreq          as W


makeFree ''HttpUriF

type HttpP = Free HttpF
type HttpUriP a = Free (HttpUriF a)
type HttpHeaderP a = Free (HttpHeaderF a)


get :: (Aeson.FromJSON a) => W.Options -> String -> Free HttpF (Maybe a)
get opts u = Aeson.decode <$> getRaw opts u


getRaw :: W.Options -> String -> Free HttpF D.ByteString
getRaw opts u = liftF (Get opts u id)


getRawUrl
  :: forall x.
      W.Options
   -> (x -> String)
   -> HttpUriP x (Either HttpError D.ByteString)

getRawUrl = getUri
-- getRawUrl opts f
--   = liftF (GetUri opts f $ \x ->
--       case x of
--         Left e -> _
--         Right v -> v
--           )


getRawOpts :: (a -> W.Options) -> String -> HttpHeaderP a D.ByteString
getRawOpts f u = liftF (GetOptions f u id)
