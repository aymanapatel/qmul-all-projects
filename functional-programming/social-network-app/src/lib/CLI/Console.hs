module CLI.Console
  ( -- * Methods to print in standard output
    printResults,
    printSpamStats,
  )
where

import Control.Concurrent.MVar
import Control.Monad (forM_)
import Data.List (find, sortOn)
import Filter.SpamDetector (SpamState, getSpamStats)
import Types.Message (Message (..))
import Types.Spam (SpamState (..), SpamStateData (..), SpamStats (..))
import Types.User (User (..))

-- | Prints the final results of the simulation.
--
-- * @users@: List of users to print results for.
-- * @spamState@: The spam detection state to retrieve spam messages from.
printResults :: [User] -> SpamState -> IO ()
printResults users (SpamState spamStateVar) = do
  putStrLn "=============================================="
  putStrLn "          Simulation Results (Basic Feature)"
  putStrLn "=============================================="
  putStrLn ""

  -- Helper to get user name by ID
  let getUserName uid =
        maybe (show uid) userName $ find (\u -> userId u == uid) users

  -- Gather allowed messages from all inboxes
  allowedMsgs <- concat <$> mapM (\u -> readMVar (inbox u)) users

  -- Gather spam messages
  spamData <- readMVar spamStateVar
  let spamMsgs = spamMessages spamData

  -- Combine and sort all messages by time
  -- Tuple: (Message, IsSpam)
  let allMsgs =
        sortOn (msgTime . fst) $
          [(m, False) | m <- allowedMsgs] ++ [(m, True) | m <- spamMsgs]

  putStrLn "Message History:"
  forM_ allMsgs $ \(msg, isSpam) -> do
    let preFix = if isSpam then "❌ [SPAM] " else "✅ [NOT SPAM]"
        sender = getUserName (msgFrom msg)
        recipient = getUserName (msgTo msg)
        time = show (msgTime msg)
        content = msgContent msg

    putStrLn $ preFix ++ "[" ++ time ++ "] " ++ sender ++ " -> " ++ recipient ++ ": " ++ content

  putStrLn ""

  putStrLn "Message Counts per User:"
  forM_ users $ \user -> do
    count <- readMVar (receivedCount user)
    putStrLn $ "  - " ++ userName user ++ ": " ++ show count ++ " messages received"

  putStrLn ""

-- | Prints spam detection statistics.
--
-- * @spamState@: The spam detection state to display statistics from.
printSpamStats :: SpamState -> IO ()
printSpamStats spamState = do
  stats <- getSpamStats spamState
  putStrLn "----------------------------------------------"
  putStrLn "     Spam Detection Statistics(Extra feature) "
  putStrLn "----------------------------------------------"
  putStrLn $ "  Total messages checked: " ++ show (totalMessagesChecked stats)
  putStrLn $ "  Messages allowed:       " ++ show (messagesAllowed stats)
  putStrLn $ "  Spam detected:          " ++ show (spamDetected stats)
  putStrLn ""
  putStrLn "=============================================="