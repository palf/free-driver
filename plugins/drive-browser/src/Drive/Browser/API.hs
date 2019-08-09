{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Drive.Browser.API
  where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Drive.Browser.Types


makeFree ''BrowserF
