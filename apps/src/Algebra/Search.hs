{-# LANGUAGE MultiParamTypeClasses #-}
module Algebra.Search
  ( SearchF
  , searchFor
  , findFirstMatch
  ) where

import qualified Control.Monad.Free as Free
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Drive              as D


data SearchF a
  = SearchFor Text a
  | FindFirstMatch (Text -> a)
  deriving (Functor)


searchFor :: Text -> Free SearchF ()
searchFor t = Free.liftF $ SearchFor t ()


findFirstMatch :: Free SearchF Text
findFirstMatch = Free.liftF $ FindFirstMatch id


searchToLogI :: D.Interpreter SearchF D.LogF a
searchToLogI (SearchFor t a)    = D.debug ("searchfor: " <> t) >> pure a
searchToLogI (FindFirstMatch a) = D.debug "findfirstmatch" >> pure (a "now")


searchToBrowserI :: D.Interpreter SearchF D.BrowserF a
searchToBrowserI (SearchFor t a) = do
  D.goToUrl (D.Url "http://www.google.com")
  D.sendText (D.CSS ".gsfi") t
  D.pressEnter (D.CSS ".gsfi")
  D.sleep 2
  pure a

searchToBrowserI (FindFirstMatch a)
  = a <$> D.readText (D.CSS "h3.r")


instance D.Interpretable SearchF D.BrowserF where
  interpret = searchToBrowserI


instance D.Interpretable SearchF D.LogF where
  interpret = searchToLogI
