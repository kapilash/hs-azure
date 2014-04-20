module Main where

import Web.WindowsAzure.ServiceBus.Queue
import Web.WindowsAzure.ServiceBus.SBTypes
import qualified Data.ByteString.Char8 as C

main = do
  sbContext <- sbContext (simpleSBInfo "sb-namespace" "issuer-key")
  enQueueBS "queue-name" (C.pack "hello Haskell world") sbContext
  res <- deQueue "queue-name" 30 sbContext
  print res