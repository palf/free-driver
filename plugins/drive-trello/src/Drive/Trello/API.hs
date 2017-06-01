{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Drive.Trello.API
   where

import Control.Monad.Free.TH
import Control.Monad.Free
import Drive.Trello.Types


makeFree ''TrelloF
