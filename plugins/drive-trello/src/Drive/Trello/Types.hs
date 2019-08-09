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

import qualified Data.Aeson   as A
import qualified Data.Text    as T

import           Data.Aeson   ((.:))
import           Data.Text    (Text)
import           GHC.Generics


newtype User = User T.Text
newtype Organization = Organization T.Text

newtype Board = Board
  { boardName :: T.Text
  } deriving (Show, Eq)

instance A.FromJSON Board where
  parseJSON = A.withObject "board" $
    \v -> Board <$> v .: "name"


data TrelloF a
  = GetBoards User ([Board] -> a)
  deriving (Functor)


data TrelloAuth = TrelloAuth
  { user  :: Text
  , token :: Text
  , key   :: Text
  } deriving (Show, Eq, Generic)

instance A.FromJSON TrelloAuth
