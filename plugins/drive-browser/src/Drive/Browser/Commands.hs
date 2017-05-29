{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE KindSignatures #-}

module Drive.Browser.Commands
  ( openPage
  , refresh
  , readTitle
  , click
  , clear
  , sendKeys
  , pressEnter
  , getText
  , delay
  ) where

import qualified Control.Concurrent             as Cr
import qualified Control.Monad.IO.Class         as IOC
import qualified Data.Text                      as T
import qualified Test.WebDriver.Class           as W.Cl
import qualified Test.WebDriver.Commands        as W.Com
import qualified Test.WebDriver.Common.Keys     as W.Keys

import Drive.Browser.Ref

openPage :: (W.Cl.WebDriver m) => String -> m ()
openPage = W.Com.openPage


refresh :: (W.Cl.WebDriver m) => m ()
refresh = W.Com.refresh


readTitle :: (W.Cl.WebDriver m) => m T.Text
readTitle = W.Com.getTitle


click :: (W.Cl.WebDriver m) => Ref -> m ()
click r = withElement r W.Com.click


clear :: (W.Cl.WebDriver m) => Ref -> m ()
clear r = withElement r W.Com.clearInput


sendKeys :: (W.Cl.WebDriver m) => Ref -> T.Text -> m ()
sendKeys r t = withElement r (W.Com.sendKeys t)


pressEnter :: (W.Cl.WebDriver m) => Ref -> m ()
pressEnter r = withElement r (W.Com.sendKeys W.Keys.enter)


getText :: (W.Cl.WebDriver m) => Ref -> m T.Text
getText r = withElement r W.Com.getText


delay :: (IOC.MonadIO m) => Int -> m ()
delay n = IOC.liftIO $ Cr.threadDelay (1000000 * n)

-- private

withElement
  :: forall (m :: * -> *) b. (W.Cl.WebDriver m)
  => Ref
  -> (W.Com.Element -> m b) -> m b

withElement r op = W.Com.findElem (selector r) >>= op


selector :: Ref -> W.Com.Selector
selector (CSS t) = W.Com.ByCSS t
selector (LinkText t) = W.Com.ByLinkText t
