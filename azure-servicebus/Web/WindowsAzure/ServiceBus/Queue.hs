-- |
-- Module : Web.WindowsAzure.ServiceBus.Queue
-- Description : API for reading from and writing to ServiceBus Queue
-- Copyright : (c) Hemanth Kapila, 2014
-- License : BSD3
-- Maintainer : saihemanth@gmail.com
-- Stability  : Experimental
--
-- Provides API to pull from and push to ServiceBus queue
-- Please refer to <http://msdn.microsoft.com/en-us/library/hh780726.aspx Service Bus Rest API> for Service bus API
--
-- Following piece of code illustrates the use of API
--
-- @
-- module Main where
-- 
-- import Web.WindowsAzure.ServiceBus.SBTypes
-- import Web.WindowsAzure.ServiceBus.Queue
-- import qualified Data.ByteString.Char8 as C
-- 
-- main = do
--   sbContext <- sbContext (simpleSBInfo "sb-namespace" "insert-your-issuer-key")
--   enQueueBS sbContext "queue-name" (C.pack "hello Haskell world")
--   res <- deQueue sbContext "kqueue" 30
--   print res
-- @
-- 

module Web.WindowsAzure.ServiceBus.Queue( 
  -- * Pushing data to Queue
  enQueueBS,
  enQueueLBS,
  enQueueBodySrc,
  -- * Reading data from Queue
  deQueue
   )where

import Web.WindowsAzure.ServiceBus.SBTypes
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit hiding (requestBodySource)
import Network.HTTP.Client.Conduit 
import Data.Conduit
import Data.Int

-- | publish a message containing 'C.ByteString' to queue.
--
-- The following publishes a strict bytestring \bs\ to queue \q\
--
-- @
-- enQueueBS q bs ctx
-- @
enQueueBS ::  String -> C.ByteString -> SBContext -> IO ()
enQueueBS   queueName content context = 
  enQueueRequest  queueName (RequestBodyBS content) context
  
  
-- | publish a message containing 'L.ByteString' to queue
--  
--  The following publishes a lazy bytestring ,\lbs\, to queue \q\,  
--  
-- @  
-- enQueueLBS q lbs  ctx
-- @  
--  
enQueueLBS :: String -> L.ByteString -> SBContext -> IO ()  
enQueueLBS  queueName content context = 
  enQueueRequest  queueName (RequestBodyLBS content) context
  
  
-- | publish from a 'Source' (refer to 'requestBodySource')
enQueueBodySrc ::  String -> Int64 -> Source IO C.ByteString -> SBContext -> IO ()
enQueueBodySrc  queueName  len bodysrc context =   enQueueRequest queueName (requestBodySource len bodysrc) context

  
-- | Reads and deletes the message from a queue. 
--  
--  
-- In order to destructively read the latest message from the queue (with a time out of n seconds),   
--  
--  
-- @  
-- deQueue queueName n context
-- @
--  Note that the timeout can be at the most 55 seconds. This silently \ignores\ the timeouts greater than 55  
deQueue :: String -> Int -> SBContext -> IO (L.ByteString)
deQueue  queueName timeout context =  deQueueRequest  queueName (timeout `mod` 55) context

