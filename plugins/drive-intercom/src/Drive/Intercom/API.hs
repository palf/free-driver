{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Drive.Intercom.API
   where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Drive.Intercom.Types


makeFree ''IntercomF
