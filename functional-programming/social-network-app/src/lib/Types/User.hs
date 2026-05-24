module Types.User
  ( -- * User Types
    User (..),
    UserId,
    createUser,
  )
where

import Control.Concurrent.MVar
import Types.Common (UserId)
import Types.Message (Message)

-- | Represents a user in the social network.
data User = User
  { -- | Unique identifier for the user
    userId :: UserId,
    -- | Display name of the user
    userName :: String,
    -- | Inbox for received messages (protected by MVar)
    inbox :: MVar [Message],
    -- | Number of messages received (protected by MVar)
    receivedCount :: MVar Int
  }

-- | Necessary for comparing users
instance Eq User where
  u1 == u2 = userId u1 == userId u2

-- | Creates a new user with the given ID and name.
-- Initializes an empty inbox and zero received count using MVar.
--
-- * @uid@: The unique ID to assign to the user.
-- * @name@: The display name of the user.
createUser :: UserId -> String -> IO User
createUser uid name = do
  inb <- newMVar []
  cnt <- newMVar 0
  return
    User
      { userId = uid,
        userName = name,
        inbox = inb,
        receivedCount = cnt
      }