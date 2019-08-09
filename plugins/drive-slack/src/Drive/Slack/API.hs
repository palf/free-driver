{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Drive.Slack.API
  where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Drive.Slack.Types


makeFree ''SlackF
