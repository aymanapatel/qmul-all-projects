module Constant.Constant
  ( -- * User messages Constants
    maxMessages,
    messageContents,
    userNames,

    -- * Spam configuration constants
    spamWindowSeconds,
    spamThreshold,

    -- * Simulation timing constants
    minUserDelay,
    maxUserDelay,
  )
where

-- | Maximum number of messages to be sent in the simulation.
maxMessages :: Int
maxMessages = 100

-- | List of possible message contents.
messageContents :: [String]
messageContents =
  [ "Hello, how are you?",
    "What's up?",
    "Have you seen the latest news?",
    "Do you want to grab a coffee?",
    "Let's catch up soon!",
    "I found this interesting article...",
    "Did you watch the game last night?",
    "Can you believe this weather?",
    "I'm planning a trip, got any tips?",
    "Just finished reading a great book!"
  ]

-- | List of possible user names.
userNames :: [(Int, String)]
userNames =
  [ (1, "Alice"),
    (2, "Bob"),
    (3, "Charlie"),
    (4, "Diana"),
    (5, "Eve"),
    (6, "Frank"),
    (7, "Grace"),
    (8, "Heidi"),
    (9, "Ivan"),
    (10, "Judy")
  ]

-- Extra feature

-- | Time window for spam detection (in seconds).
-- If a user sends more than spamThreshold messages within this window, it's spam.
spamWindowSeconds :: Double
spamWindowSeconds = 0.2

-- | Maximum messages allowed from same sender to same recipient within the window.
spamThreshold :: Int
spamThreshold = 2

-- | Minimum random delay between messages (in microseconds).
minUserDelay :: Int
minUserDelay = 10000

-- | Maximum random delay between messages (in microseconds).
maxUserDelay :: Int
maxUserDelay = 50000