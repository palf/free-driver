module Drive.Trello.API
  ( getBoards
  ) where

import Drive.Trello.Types
import Drive (Free, liftF)


getBoards :: User -> Free TrelloF [Board]
getBoards u = liftF $ GetBoards u id
