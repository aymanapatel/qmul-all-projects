module SocialNetwork.Simulation
  ( runSimulation,
    generateRandomMessage,
    userThread,
    waitForCompletion,
  )
where

import CLI.Console (printResults, printSpamStats)
import Constant.Constant (maxMessages, maxUserDelay, messageContents, minUserDelay, userNames)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forM_, when)
import Data.Time.Clock (getCurrentTime)
import Filter.SpamDetector (SpamState, createSpamState, trySendMessageWithSpamCheck)
import System.Random (randomRIO)
import Types.Message (Message (..))
import Types.User (User (..), UserId, createUser)

-- | Sends a message from one user to another.
-- Adds the message to the recipient's inbox and increments their count.
sendMessage :: User -> Message -> IO ()
sendMessage recipient msg = do
  msgs <- takeMVar (inbox recipient)
  putMVar (inbox recipient) (msg : msgs)
  count <- takeMVar (receivedCount recipient)
  putMVar (receivedCount recipient) (count + 1)

-- | Generates a random message from sender to recipient.
-- Selects random content from predefined lists.
--
-- * @fromId@: The ID of the sending user.
-- * @toId@: The ID of the recipient user.
generateRandomMessage :: UserId -> UserId -> IO Message
generateRandomMessage fromId toId = do
  contentIdx <- randomRIO (0, length messageContents - 1)
  timestamp <- getCurrentTime
  return
    Message
      { msgFrom = fromId,
        msgTo = toId,
        msgContent = messageContents !! contentIdx,
        msgTime = timestamp
      }

-- | User thread behavior.
-- Each user randomly selects another user and sends them a message at random intervals.
-- Continues until the done signal is set or max messages reached.
-- Messages are filtered through spam detection before sending.
--
-- * @allUsers@: List of all users in the system.
-- * @currentUser@: The user this thread represents.
-- * @msgCounter@: Shared counter for total messages sent.
-- * @doneSignal@: MVar used to signal when the simulation should end.
-- * @spamState@: The state for the spam detection filter.
userThread :: [User] -> User -> MVar Int -> MVar Bool -> SpamState -> IO ()
userThread allUsers currentUser msgCounter doneSignal spamState = loop
  where
    -- Design Decision: Self-exclusion
    otherUsers = filter (/= currentUser) allUsers

    loop = do
      -- Check if we should stop
      shouldStop <- readMVar doneSignal
      when (not shouldStop) $ do
        -- Design Decision: Random delay between 10-100 milliseconds
        delay <- randomRIO (minUserDelay, maxUserDelay)
        threadDelay delay

        -- Check again after delay
        shouldStop' <- readMVar doneSignal
        when (not shouldStop') $ do
          -- Try to send a message
          currentCount <- takeMVar msgCounter
          sent <-
            if currentCount >= maxMessages
              then do
                putMVar msgCounter currentCount
                -- Set done signal if not already set
                _ <- swapMVar doneSignal True
                return False
              else do
                putMVar msgCounter (currentCount + 1)
                return True

          when sent $ do
            trySendMessageWithSpamCheck currentUser otherUsers spamState generateRandomMessage sendMessage
            loop

-- | Runs the main simulation.
-- Creates users, spawns threads, waits for completion, and outputs results.
runSimulation :: IO ()
runSimulation = do
  putStrLn "=============================================="
  putStrLn "    Social Network Simulation Starting..."
  putStrLn "=============================================="
  putStrLn ""

  -- Create users
  users <- mapM (uncurry createUser) userNames

  -- Create shared state
  msgCounter <- newMVar 0
  doneSignal <- newMVar False

  -- Create spam detection state
  spamState <- createSpamState

  putStrLn $ "Created " ++ show (length users) ++ " users:"
  forM_ users $ \u -> putStrLn $ "  - " ++ userName u ++ " (ID: " ++ show (userId u) ++ ")"
  putStrLn ""
  putStrLn "Starting message exchange..."
  putStrLn ""

  -- Spawn user threads with spam filter
  forM_ users $ \u ->
    forkIO $ userThread users u msgCounter doneSignal spamState

  -- Wait for simulation to complete
  waitForCompletion doneSignal

  -- Small delay to ensure all messages are processed
  threadDelay 200000

  -- Output results
  printResults users spamState
  printSpamStats spamState

-- | Waits for the simulation to complete by polling the done signal.
--
-- * @doneSignal@: MVar that becomes true when the message limit is reached.
waitForCompletion :: MVar Bool -> IO ()
waitForCompletion doneSignal = loop
  where
    loop = do
      threadDelay 100000 -- Poll every 100ms
      done <- readMVar doneSignal
      if done
        then return ()
        else loop