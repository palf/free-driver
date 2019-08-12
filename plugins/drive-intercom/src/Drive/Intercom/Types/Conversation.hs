{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Drive.Intercom.Types.Conversation
  ( ConversationsResponse (..)
  , ConversationPart (..)
  , ConversationID (..)
  , Conversation (..)
  , nullConversation
  ) where

import           Data.Aeson.Types
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import           GHC.Generics                    (Generic)

import           Drive.Intercom.Types.Pagination


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
  parseJSON = withObject "author" $
    \v -> v .: "type"
    >>= \case
      ("admin" :: String) -> KnownAdmin <$> (v .: "id")
      _ -> pure Anon


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

    p <- v .:? "conversation_parts" >>= \case
        Nothing -> pure []
        Just ps'
          -> ps' .: "conversation_parts"
          >>= withArray "Conversation parts" extractParts

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
