{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Drive.Terminal.API
   where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Drive.Terminal.Types


makeFree ''TerminalF

type TerminalP = Free TerminalF
