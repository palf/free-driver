{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Drive.Browser.Commands
  ( W.Com.openPage
  , W.Com.refresh
  , readTitle
  , click
  , clear
  , sendKeys
  , pressEnter
  , getText
  , delay
  ) where

import qualified Control.Concurrent         as Cr
import qualified Control.Monad.IO.Class     as IOC
import           Data.Text                  (Text)
import           Drive.Browser.Ref
import qualified Test.WebDriver.Class       as W.Cl
import qualified Test.WebDriver.Commands    as W.Com
import qualified Test.WebDriver.Common.Keys as W.Keys

readTitle :: (W.Cl.WebDriver m) => m Text
readTitle = W.Com.getTitle


click :: (W.Cl.WebDriver m) => Ref -> m ()
click r = withElement r W.Com.click


clear :: (W.Cl.WebDriver m) => Ref -> m ()
clear r = withElement r W.Com.clearInput


sendKeys :: (W.Cl.WebDriver m) => Ref -> Text -> m ()
sendKeys r t = withElement r (W.Com.sendKeys t)


pressEnter :: (W.Cl.WebDriver m) => Ref -> m ()
pressEnter r = withElement r (W.Com.sendKeys W.Keys.enter)


getText :: (W.Cl.WebDriver m) => Ref -> m Text
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
selector (CSS t)      = W.Com.ByCSS t
selector (LinkText t) = W.Com.ByLinkText t
