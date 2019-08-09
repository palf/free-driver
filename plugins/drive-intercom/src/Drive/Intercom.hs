module Drive.Intercom
  ( module X
  ) where

import           Drive.Intercom.API      as X
import           Drive.Intercom.Handlers as X
import           Drive.Intercom.Types    as X


-- getConversationsResponse
--   :: IO C.ConversationsResponse

-- getConversationsResponse = do
--   r <- W.get opts "https://api.intercom.io/conversations"
--   -- let r' = eitherDecode r :: Either String C.ConversationsResponse
--   --
--   case eitherDecode r of
--     Left _e -> C.ConversationsResponse Nothing []
--     Right v -> v


-- getNextConvPage :: C.ConversationsResponse -> C.ConversationsResponse
-- getNextConvPage r
--   = case C.pages r of
--       Nothing -> Nothing
--       Just p -> getConversationPage (P.next p)


-- --   case C.pages cr of
-- --     Nothing -> pure cr
-- --     Just p -> do
-- --       let hasNext = isJust (P.next p)
-- --       if hasNext
-- --         then getNext (fromJust (P.next p))
-- --         else pure cr


