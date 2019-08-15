{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

module Drive.Browser.Types
  ( BrowserF (..)
  , Url (..)
  , SupportsBrowser
  , browserToDescribeI
  , execBrowser
  ) where

import qualified Control.Monad.IO.Class as IOC
import           Data.Aeson
import           Data.Functor           (($>))
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Drive
import qualified Drive                  as D
import qualified Drive.Browser.Commands as C
import           Drive.Browser.Ref
import           Drive.Describe
import qualified Test.WebDriver.Class   as W.Cl


newtype Url
  = Url String

deriving instance Show Url
deriving instance Eq Url
deriving instance ToJSON Url


data BrowserF a
  = GoToUrl Url a
  | Refresh a
  | ReadTitle (Text -> a)
  | ClickOn Ref a
  | Clear Ref a
  | SendText Ref Text a
  | PressEnter Ref a
  | ReadText Ref (Text -> a)
  | Sleep Int a
  deriving (Functor)

instance (Show a) => Show (BrowserF a) where
  show (GoToUrl u a) = "GoToUrl (" ++ show u ++ ") " ++ show a
  show _             = undefined

instance (Eq a) => Eq (BrowserF a) where
  (==) (GoToUrl u1 a1) (GoToUrl u2 a2) = (u1 == u2) && (a1 == a2)
  (==) _ _                             = undefined

instance (ToJSON a) => ToJSON (BrowserF a) where
  toJSON = undefined


browserToDescribeI :: Interpreter BrowserF DescribeF a
browserToDescribeI (GoToUrl (Url u) a) = logGoToUrl u    $> a
browserToDescribeI (Refresh a)         = logRefresh      $> a
browserToDescribeI (ReadTitle a)       = logReadTitle    $> a "some page title"
browserToDescribeI (ClickOn r a)       = logClick r      $> a
browserToDescribeI (Clear r a)         = logClear r      $> a
browserToDescribeI (SendText r t a)    = logSendText r t $> a
browserToDescribeI (PressEnter r a)    = logPressEnter r $> a
browserToDescribeI (ReadText r a)      = logReadText r   $> a "from read text"
browserToDescribeI (Sleep n a)         = logSleep n      $> a


logGoToUrl :: String -> D.Free DescribeF ()
logGoToUrl u
  = verbose ("open (" <> Text.pack u <> ")")

logRefresh :: D.Free DescribeF ()
logRefresh
  = verbose "refresh"

logReadTitle :: D.Free DescribeF ()
logReadTitle
  = verbose "read page title"

logClick :: Ref -> D.Free DescribeF ()
logClick r
  = verbose ("click (" <> showRef r <> ")")

logClear :: Ref -> D.Free DescribeF ()
logClear r
  = verbose ("clear (" <> showRef r <> ")")

logSendText :: Ref -> Text -> D.Free DescribeF ()
logSendText r t
  = verbose ("sendtext (" <> showRef r <> ", " <> t <> ")")

logPressEnter :: Ref -> D.Free DescribeF ()
logPressEnter r
  = verbose ("pressenter (" <> showRef r <> ")")

logReadText :: Ref -> D.Free DescribeF ()
logReadText r
  = verbose ("readtext (" <> showRef r <> ")")

logSleep :: Int -> D.Free DescribeF ()
logSleep n
  = warn ("sleep (" <> Text.pack (show n) <> ")")


type SupportsBrowser m = (W.Cl.WebDriver m, IOC.MonadIO m)


execBrowser :: (SupportsBrowser m) => BrowserF a -> m a
execBrowser (GoToUrl (Url u) a) = a <$  C.openPage u
execBrowser (Refresh a)         = a <$  C.refresh
execBrowser (ReadTitle a)       = a <$> C.readTitle
execBrowser (ClickOn r a)       = a <$  C.click r
execBrowser (Clear r a)         = a <$  C.clear r
execBrowser (SendText r t a)    = a <$  C.sendKeys r t
execBrowser (PressEnter r a)    = a <$  C.pressEnter r
execBrowser (ReadText r a)      = a <$> C.getText r
execBrowser (Sleep n a)         = a <$  C.delay n
