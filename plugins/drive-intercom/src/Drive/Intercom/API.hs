{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Drive.Intercom.API
   where

import Control.Monad.Free.TH
import Control.Monad.Free
import Drive.Intercom.Types


makeFree ''IntercomF
