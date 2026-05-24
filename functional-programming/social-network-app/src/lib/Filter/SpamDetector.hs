module Filter.SpamDetector
  ( -- * Spam Detection
    SpamState,
    createSpamState,
    checkAndRecordMessage,
    getSpamStats,
    trySendMessageWithSpamCheck,
  )
where

import Constant.Constant (spamThreshold, spamWindowSeconds)
import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.Random (randomRIO)
import Types.Common (UserId)
import Types.Message (Message (..))
import Types.Spam (FilterResult (..), SpamState (..), SpamStateData (..), SpamStats (..))
import Types.User (User(..))

-- | Creates a new spam detection state.
createSpamState :: IO SpamState
createSpamState = do
  stateVar <-
    newMVar
      SpamStateData
        { messageHistory = Map.empty,
          totalChecked = 0,
          spamCount = 0,
          allowedCount = 0,
          spamMessages = []
        }
  return (SpamState stateVar)

-- * @fromId@: The ID of the sending user.

-- * @toId@: The ID of the recipient user.

-- * @msg@: The message to check.

-- * @spamState@: The current spam detection state.

checkAndRecordMessage :: UserId -> UserId -> Message -> SpamState -> IO FilterResult
checkAndRecordMessage fromId toId msg (SpamState stateVar) = do
  now <- getCurrentTime
  state <- takeMVar stateVar

  -- Get existing timestamps for this sender-recipient pair
  let key = (fromId, toId)
      existingTimes = Map.findWithDefault [] key (messageHistory state)

      -- Filter to only recent timestamps (within window)
      recentTimes = filter (isRecent now) existingTimes

      -- Check if adding this message would exceed threshold
      isSpamMessage = length recentTimes >= spamThreshold

  if isSpamMessage
    then do
      -- Spam detected: update counts and record the spam message
      let newState =
            state
              { totalChecked = totalChecked state + 1,
                spamCount = spamCount state + 1,
                spamMessages = msg : spamMessages state
              }
      putMVar stateVar newState
      return Spam
    else do
      -- Allowed: record the timestamp
      let newTimes = now : recentTimes
          newHistory = Map.insert key newTimes (messageHistory state)
          newState =
            state
              { messageHistory = newHistory,
                totalChecked = totalChecked state + 1,
                allowedCount = allowedCount state + 1
              }
      putMVar stateVar newState
      return Allowed
  where
    -- Check if timestamp is within the spam window
    isRecent :: UTCTime -> UTCTime -> Bool
    isRecent now t = diffUTCTime now t < realToFrac spamWindowSeconds

-- | Gets the current spam statistics.
--
-- * @spamState@: The spam detection state to query.
getSpamStats :: SpamState -> IO SpamStats
getSpamStats (SpamState stateVar) = do
  state <- readMVar stateVar
  return
    SpamStats
      { totalMessagesChecked = totalChecked state,
        spamDetected = spamCount state,
        messagesAllowed = allowedCount state
      }

-- | Attempts to send a message with spam checking.
-- Selects a random recipient, generates a message, checks spam filter,
-- and sends only if allowed.
--
-- * @currentUser@: The user sending the message.
-- * @otherUsers@: List of potential recipients.
-- * @spamState@: The spam detection state.
-- * @generateMessageFn@: Function to generate a message given from and to UserIds.
-- * @sendMessageFn@: Function to send a message to a recipient.
trySendMessageWithSpamCheck ::
  User ->
  [User] ->
  SpamState ->
  (UserId -> UserId -> IO Message) ->
  (User -> Message -> IO ()) ->
  IO ()
trySendMessageWithSpamCheck currentUser otherUsers spamState generateMessageFn sendMessageFn = do
  -- Select random recipient
  recipientIdx <- randomRIO (0, length otherUsers - 1)
  let recipient = otherUsers !! recipientIdx

  -- Generate message first (so we can check its content/metadata)
  msg <- generateMessageFn (userId currentUser) (userId recipient)

  -- Check spam filter before sending
  filterResult <- checkAndRecordMessage (msgFrom msg) (msgTo msg) msg spamState
  case filterResult of
    Allowed -> do
      -- Send message if allowed
      sendMessageFn recipient msg
    Spam ->
      -- Message rejected as spam
      return ()
