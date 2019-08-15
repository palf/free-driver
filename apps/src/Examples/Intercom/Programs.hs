{-# LANGUAGE OverloadedStrings #-}

module Examples.Intercom.Programs
  ( fetchConversation
  , fetchUser
  , fetchAdmin
  , fetchAllAdmins
  , fetchAllConversations
  , firstConversation
  ) where

import           Data.Text      (Text)
import qualified Drive          as D
import qualified Drive.Intercom as I


type IntercomP a = D.Free I.IntercomF a


fetchConversation :: Text -> IntercomP I.Conversation
fetchConversation c
  = I.getConversation (I.ConversationID c)


fetchUser :: Text -> IntercomP I.User
fetchUser c
  = I.getUser (I.UserID c)


fetchAdmin :: Text -> IntercomP Text
fetchAdmin c
  = I.adminName <$> I.getAdmin (I.AdminID c)


fetchAllAdmins :: IntercomP [Text]
fetchAllAdmins
  = (fmap . fmap) I.adminName I.listAdmins


fetchAllConversations :: IntercomP [I.ConversationID]
fetchAllConversations
  = I.listConversations


firstConversation :: IntercomP (Either Text I.Conversation)
firstConversation = do
  cs <- I.listConversations
  case cs of
    []  -> pure $ Left "did not get any conversations"
    x:_ -> Right <$> I.getConversation x
