{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Drive.Trello.Types
  ( TrelloF (..)
  , TrelloAuth (..)

  , User (..)
  , Organization (..)
  , Board (..)
  ) where

import qualified Data.Text as T
import GHC.Generics
import Data.Aeson


newtype User = User T.Text
newtype Organization = Organization T.Text

newtype Board = Board
  { boardName :: T.Text
  } deriving (Show, Eq)

instance FromJSON Board where
  parseJSON = withObject "board" $ \v -> do
    n <- v .: "name"

    pure $ Board n


data TrelloF a
  = GetBoards User ([Board] -> a)
  deriving (Functor)


data TrelloAuth = TrelloAuth
  { token :: T.Text
  , key :: T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON TrelloAuth
