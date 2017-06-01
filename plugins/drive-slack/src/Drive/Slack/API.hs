{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Drive.Slack.API
  where

import Control.Monad.Free.TH
import Control.Monad.Free
import Drive.Slack.Types


makeFree ''SlackF
