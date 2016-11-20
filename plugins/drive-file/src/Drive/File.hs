{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Drive.File
  ( FileF (..)
  , write
  , withFile
  , execFile
  ) where

import qualified Control.Monad.Free     as F
import qualified Control.Monad.IO.Class as IOC
import qualified Control.Monad.Reader   as R
import qualified Data.Text              as T

import Data.Semigroup ((<>))


data FileF a
  = WriteFile T.Text a
  deriving (Functor)


write :: T.Text -> F.Free FileF ()
write t = F.liftF $ WriteFile t ()


type SupportsFile m = (R.MonadReader FilePath m, IOC.MonadIO m)


withFile :: FilePath -> R.ReaderT FilePath m a -> m a
withFile = flip R.runReaderT


execFile :: (SupportsFile m) => FileF a -> m a
execFile (WriteFile t a) = do
  path <- R.ask
  IOC.liftIO $ appendFile path (T.unpack $ t <> "\n")
  pure a
