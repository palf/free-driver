{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Drive.HTTP.API
  ( W.defaults
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


get :: (Aeson.FromJSON a) => W.Options -> String -> Free HttpF (Maybe a)
get opts u = Aeson.decode <$> getRaw opts u


getRaw :: W.Options -> String -> Free HttpF D.ByteString
getRaw opts u = liftF (Get opts u id)


getRawUrl
  :: forall x.
      W.Options
   -> (x -> String)
   -> Free (HttpUriF x) (Either HttpError D.ByteString)

getRawUrl = getUri
-- getRawUrl opts f
--   = liftF (GetUri opts f $ \x ->
--       case x of
--         Left e -> _
--         Right v -> v
--           )


getRawOpts :: (y -> W.Options) -> String -> Free (HttpHeaderF y) D.ByteString
getRawOpts f u = liftF (GetOptions f u id)
