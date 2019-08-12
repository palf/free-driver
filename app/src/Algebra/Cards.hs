module Drive.Cards
  ( CardsF
  , Game (..)
  , open
  , select
  , readHeader
  ) where

import qualified Data.Text          as T
import qualified Drive         as D

import Data.Monoid ((<>))


data CardsF a
  = Open a
  | Select Game a
  | ReadHeader (T.Text -> a)
  deriving (Functor)


data Game = Solitaire | Freecell | Spider
  deriving (Show)


open :: D.Free CardsF ()
open = D.liftF $ Open ()


select :: Game -> D.Free CardsF ()
select t = D.liftF $ Select t ()


readHeader :: D.Free CardsF T.Text
readHeader = D.liftF $ ReadHeader id


instance D.Interpretable CardsF D.LogF where
  interpret = cardsToLogI


cardsToLogI :: D.Interpreter CardsF D.LogF a
cardsToLogI (Open a)       = D.debug (T.pack "open") >> pure a
cardsToLogI (Select t a)   = D.debug (T.pack $ "select " <> show t) >> pure a
cardsToLogI (ReadHeader a) = D.debug (T.pack "readHeader") >> pure (a "header")


instance D.Interpretable CardsF D.BrowserF where
  interpret = cardsToBrowserI


cardsToBrowserI :: D.Interpreter CardsF D.BrowserF a
cardsToBrowserI (Open a) = do
  D.goToUrl (D.Url "http://localhost:3000")
  pure a

cardsToBrowserI (Select t a) = do
  D.clickOn (D.LinkText $ T.pack (show t) )
  pure a

cardsToBrowserI (ReadHeader a)
  = a <$> D.readText (D.CSS "h3")
