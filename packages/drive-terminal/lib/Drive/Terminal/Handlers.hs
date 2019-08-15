{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Drive.Terminal.Handlers
  ( terminalToDescribeI
  , execTerminal
  ) where

import qualified Data.Text              as Text
import qualified Drive                  as D
import qualified Drive.Describe         as D

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Functor           (($>))

import           Drive.Terminal.Types


type DescribeP   = D.Free D.DescribeF


terminalToDescribeI :: TerminalF a -> DescribeP a
terminalToDescribeI (WaitForEnter x) = D.debug "waiting for enter" >> pure x
terminalToDescribeI (PrintMessage s x) = D.debug ("printing \"" <> Text.pack s <> "\"") >> pure x
terminalToDescribeI (ReadInput x)    = D.debug "reading input" $> x ""


execTerminal :: (MonadIO m) => TerminalF a -> m a
execTerminal (WaitForEnter x)   = liftIO getLine >> pure x
execTerminal (PrintMessage s x) = liftIO (print s) >> pure x
execTerminal (ReadInput x)      = x <$> liftIO getLine
