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

import           Control.Monad.Free     (Free)
import qualified Control.Monad.Free     as Free
import qualified Control.Monad.IO.Class as IOC
import qualified Control.Monad.Reader   as R
import           Data.Text              (Text)
import qualified Data.Text              as Text


data FileF a
  = WriteFile Text a
  deriving (Functor)


write :: Text -> Free FileF ()
write t = Free.liftF $ WriteFile t ()


type SupportsFile m = (R.MonadReader FilePath m, IOC.MonadIO m)


withFile :: FilePath -> R.ReaderT FilePath m a -> m a
withFile = flip R.runReaderT


execFile :: (SupportsFile m) => FileF a -> m a
execFile (WriteFile t a) = do
  path <- R.ask
  IOC.liftIO $ appendFile path (Text.unpack $ t <> "\n")
  pure a
