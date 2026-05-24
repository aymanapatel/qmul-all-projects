module Types.Message
  ( -- * Message Types
    Message (..),
    createMessage,
  )
where

import Data.Time.Clock (UTCTime, getCurrentTime)
import Types.Common (UserId)

-- | Represents a message in the social network.
-- Contains sender, recipient, content, and timestamp.
data Message = Message
  { -- | Sender's user ID
    msgFrom :: !UserId,
    -- | Recipient's user ID
    msgTo :: !UserId,
    -- | Message content
    msgContent :: !String,
    -- | Time when the message was sent
    msgTime :: !UTCTime
  }
  deriving (Show, Eq)

-- | Creates a new message with the given parameters.
-- Generates a timestamp for the message.
--
-- * @fromId@: The ID of the sending user.
-- * @toId@: The ID of the recipient user.
-- * @content@: The text content of the message.
createMessage :: UserId -> UserId -> String -> IO Message
createMessage fromId toId content = do
  timestamp <- getCurrentTime
  return
    Message
      { msgFrom = fromId,
        msgTo = toId,
        msgContent = content,
        msgTime = timestamp
      }