{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Drive.Browser.API
  where

import Control.Monad.Free.TH
import Control.Monad.Free
import Drive.Browser.Types


makeFree ''BrowserF
