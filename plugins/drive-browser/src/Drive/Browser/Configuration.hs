{-# LANGUAGE OverloadedStrings #-}

module Drive.Browser.Configuration
  ( runDefaultSession
  , runInBrowser
  ) where

import qualified Data.Text             as T
import qualified Test.WebDriver        as W
import qualified Test.WebDriver.Config as W.Conf


runDefaultSession :: W.WD a -> IO a
runDefaultSession = runInBrowser "chrome"


runInBrowser :: T.Text -> W.WD a -> IO a
runInBrowser b = W.runSession conf . W.finallyClose
  where
    conf = W.Conf.useBrowser (W.Browser b) W.Conf.defaultConfig
