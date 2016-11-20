{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Drive.Intercom.Types.Conversation
  ( ConversationsResponse (..)
  , ConversationPart (..)
  , ConversationID (..)
  , Conversation (..)
  , nullConversation
  ) where

import Data.Aeson.Types
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Vector as V

import Drive.Intercom.Types.Pagination


newtype ConversationID
  = ConversationID T.Text
  deriving (Show, Generic, FromJSON)


data ConversationsResponse = ConversationsResponse
  { pages         :: Maybe Pagination
  , conversations :: [Conversation]
  } deriving (Show, Generic)

instance FromJSON ConversationsResponse


data ConversationPart = ConversationPart
  { body   :: Maybe String
  , author :: Author
  } deriving (Generic, Show)

instance FromJSON ConversationPart


data Author = Anon | KnownAdmin String
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Author where
  parseJSON = withObject "author" $ \v -> do
    t <- (v .: "type") :: Parser String
    i <- v .: "id"
    pure $ case t of
            "admin" -> KnownAdmin i
            _-> Anon


data Conversation = Conversation
  { convID  :: ConversationID
  , subject :: String
  , fields  :: [(Author, String)]
  } deriving (Generic, Show)


instance FromJSON Conversation where
  parseJSON = withObject "conversation" $ \v -> do

    i <- v .: "id"
    m <- v .: "conversation_message"
    j <- m .: "subject"
    t <- m .: "body"
    a <- m .: "author"

    p <- v .:? "conversation_parts" >>= \ps ->
      case ps of
        Nothing -> pure []
        Just ps' -> do
          x <- ps' .: "conversation_parts"
          p <- withArray "Conversation parts" extractParts x
          pure p

    let t' = (a, t)

    pure Conversation
      { convID  = i
      , subject = j
      , fields  = filter (\f -> snd f /= "") (t' : p)
      }


extractParts :: Array -> Parser [(Author, String)]
extractParts xs = mapM extractPartBody (V.toList xs)


extractPartBody :: Value -> Parser (Author, String)
extractPartBody = withObject "part" $ \x -> do
  a <- x .: "author"
  b <- x .:? "body" .!= ""
  pure (a, b)


nullConversation :: Conversation
nullConversation = Conversation
  { convID  = ConversationID "null"
  , subject = "no subject"
  , fields  = []
  }
