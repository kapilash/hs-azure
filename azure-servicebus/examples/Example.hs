module Main where

import Web.WindowsAzure.ServiceBus.Queue
import Web.WindowsAzure.ServiceBus
import qualified Data.ByteString.Char8 as C
import Network(withSocketsDo)

queueName = "QUEUE-NAME"
sbNamespace = "sb-namespace"
sbIssuerKey = C.pack "somekey23823872="
sbIssuerName = C.pack "owner"

sbinfo = SBInfo sbNamespace sbIssuerName sbIssuerKey 
message1 = C.pack "Hello from Haskell"
message2 = C.pack "Haskell Rocks"

main = do
  sbContext <- sbContext sbinfo
  enQueueBS queueName message1 sbContext
  -- Enqueued message 1 to queue
  res <- deQueue queueName 30 sbContext
  -- Dequeued it
  print res
  -- Enqueue it again
  enQueueBS queueName message1 sbContext
  -- EnQueue Another Message
  enQueueBS queueName message2 sbContext
  -- Peek Lock on the first message
  (lockInfo,msgFromQueue) <- peekLockQueue queueName 50 sbContext
  -- Now a deQueue would result in us getting the second message (while the first is still in the queue)
  res3 <- deQueue queueName 10 sbContext
  print res3
  -- renew lock
  renewLock queueName lockInfo sbContext
  -- delete the message
  deleteMessage queueName lockInfo sbContext

