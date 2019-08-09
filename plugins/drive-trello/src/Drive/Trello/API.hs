{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Drive.Trello.API
   where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Drive.Trello.Types


makeFree ''TrelloF
