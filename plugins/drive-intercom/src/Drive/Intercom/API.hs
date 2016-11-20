module Drive.Intercom.API
  ( listUsers
  , listAdmins
  , listConversations
  , getUser
  , getAdmin
  , getConversation
  ) where

import Drive.Intercom.Types
import Drive (Free, liftF)


listUsers :: Free IntercomF [UserID]
listUsers = liftF $ ListUsers id


listAdmins :: Free IntercomF [Admin]
listAdmins = liftF $ ListAdmins id


listConversations :: Free IntercomF [ConversationID]
listConversations = liftF $ ListConversations id


getUser :: UserID -> Free IntercomF User
getUser u = liftF $ GetUser u id


getAdmin :: AdminID -> Free IntercomF Admin
getAdmin u = liftF $ GetAdmin u id


getConversation :: ConversationID -> Free IntercomF Conversation
getConversation c = liftF $ GetConversation c id
