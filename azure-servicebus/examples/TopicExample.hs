module Main where

import Network.MicrosoftAzure.ServiceBus.Topic
import Network.MicrosoftAzure.ServiceBus
import qualified Data.ByteString.Char8 as C
import Network(withSocketsDo)

topic = "TopicName"
subs = "subsName"
sbNamespace = "SBNAMESPACE"
sbIssuerKey = C.pack "someSubscriptionKey="
sbIssuerName = C.pack "owner"

sbinfo = SBInfo sbNamespace sbIssuerName sbIssuerKey
message1 = C.pack "Hello from Haskell"
message2 = C.pack "Haskell Rocks"

main = do
  sbContext <- sbContext sbinfo
  sendTopicBS topic message1 sbContext
  -- Enqueued message 1 to queue
  res <- destructiveRead topic subs 30 sbContext
  -- Dequeued it
  print res
  -- Enqueue it again
  sendTopicBS topic message1 sbContext
  -- EnQueue Another Message
  sendTopicBS topic message2 sbContext
  -- Peek Lock on the first message
  (lockInfo,msgFromQueue) <- peekLockTopic topic subs 50 sbContext
  -- Now a destructiveRead would result in us getting the second message (while the first is still in the topic)
  res3 <- destructiveRead topic subs 10 sbContext
  print res3
  -- renew lock
  renewLock lockInfo sbContext
  -- delete the message
  deleteMessage lockInfo sbContext
