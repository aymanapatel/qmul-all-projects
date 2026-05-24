module Types.Spam
  ( -- * Spam Types
    FilterResult (..),
    SpamStats (..),
    SpamStateData (..),
    SpamState (..),
  )
where

import Control.Concurrent.MVar (MVar)
import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime)
import Types.Common (UserId)
import Types.Message (Message)

-- | Result of filtering a message.
-- Either the message is allowed through or it's detected as spam.
data FilterResult
  = -- | Message can be sent
    Allowed
  | -- | Message detected as spam and rejected
    Spam
  deriving (Show, Eq)

-- | Statistics about spam detection during the simulation.
data SpamStats = SpamStats
  { -- | Total messages that went through filter
    totalMessagesChecked :: !Int,
    -- | Number of spam messages detected
    spamDetected :: !Int,
    -- | Number of messages allowed through
    messagesAllowed :: !Int
  }
  deriving (Show, Eq)

-- | Internal state for tracking message history.
data SpamStateData = SpamStateData
  { -- | Timestamps per sender-recipient
    messageHistory :: !(Map (UserId, UserId) [UTCTime]),
    -- | Total messages checked
    totalChecked :: !Int,
    -- | Spam messages detected
    spamCount :: !Int,
    -- | Messages allowed
    allowedCount :: !Int,
    -- | List of messages detected as spam
    spamMessages :: ![Message]
  }

-- | Thread-safe spam detection state wrapped in MVar.
newtype SpamState = SpamState (MVar SpamStateData)
