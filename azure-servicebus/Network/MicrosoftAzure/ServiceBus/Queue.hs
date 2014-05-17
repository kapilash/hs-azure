-- |
-- Module : Network.MicrosoftAzure.ServiceBus.Queue
-- Description : API for reading from and writing to ServiceBus Queue
-- Copyright : (c) Hemanth Kapila, 2014
-- License : BSD3
-- Maintainer : saihemanth@gmail.com
-- Stability  : Experimental
--
-- Provides API to pull from and push to ServiceBus queue
-- Please refer to <http://msdn.microsoft.com/en-us/library/hh780726.aspx Service Bus Rest API> for information on the API provided by
-- Microsoft Service bus.
--
-- Simple example for how to use this library is as below
--
-- @
-- import Network.MicrosoftAzure.ServiceBus.Queue
-- import Network.MicrosoftAzure.ServiceBus
-- import qualified Data.ByteString.Char8 as C
--
-- queueName = "queueName"
-- sbNamespace = "namespace"
-- sbIssuerKey = C.pack "1287361251262as="
-- sbIssuerName = C.pack "owner"
--
-- sbinfo = SBInfo sbNamespace sbIssuerName sbIssuerKey
-- message = C.pack "Hello from Haskell"
--
-- main = do
--   sbContext <- sbContext sbinfo
--   enQueueBS queueName message sbContext
--   res <- deQueue queueName 30 sbContext
--   print res
-- @
--
-- see examples (available as a part of distribution) for a more detailed example.

module Network.MicrosoftAzure.ServiceBus.Queue(
  -- Locked Message Info
  QLockedMsgInfo,
  -- * Pushing data to Queue
  enQueueBS,
  enQueueLBS,
  enQueueBodySrc,
  -- * Reading data from Queue
  deQueue,
  peekLockQueue
   )where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Int

import Network.MicrosoftAzure.ACS
import Network.MicrosoftAzure.ServiceBus.SBTypes
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit hiding (requestBodySource)
import Network.HTTP.Client.Conduit hiding (httpLbs)
import Network.HTTP.Types.Method (methodDelete, methodPost,methodPut)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method

import qualified Data.CaseInsensitive as CI
import Network.Connection (TLSSettings (..))
import Network(withSocketsDo)


-- | 'QLockedMsgInfo' provides Information of the locked message from a queue.
--
data QLockedMsgInfo = QLockedMsgInfo String BrokerProperties
                       deriving (Show)

-- | Internal low-level method for performing HTTP calls.
--
-- Not exposed to the user
enQueueRequest :: String -> RequestBody -> SBContext -> IO ()
enQueueRequest queueName body (SBContext baseUrl manager aContext)  = do
  token <- acsToken manager aContext
  reqInit <- parseUrl (baseUrl ++ "/" ++ queueName ++ "/messages")
  withSocketsDo $ httpLbs (reqInit { method = methodPost,
                     requestHeaders = [token],
                     requestBody = body
                   }) manager
  return ()

-- | Internal low-level method for creating the HTTP calls. For internal use.
--
-- should be avoided by most users
deQueueRequest :: String -> Int -> SBContext -> IO L.ByteString
deQueueRequest queueName timeout (SBContext baseUrl manager aContext) = do
  token <- acsToken manager aContext
  reqInit <- parseUrl (baseUrl ++ "/" ++ queueName ++ "/messages/head?timeout=" ++ (show timeout))
  res <-withSocketsDo $  httpLbs ( reqInit { method = methodDelete,
                     requestHeaders = [token]
                   }) manager
  return $ responseBody res

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

-- | Peek Lock Message from a Queue. Non-Destructive Read.
--
-- Atomically retrieves the next message from a queue and locks it for further processing. The message is guaranteed not to be delivered to
-- other receivers (on the same subscription) during the duration of the lock.
--
-- Refer <http://msdn.microsoft.com/en-us/library/hh780735.aspx ServiceBus documentation> for semantics of the underlying REST API.
peekLockQueue :: String -> Int -> SBContext -> IO (LockedMsgInfo,L.ByteString)
peekLockQueue qName timeout (SBContext baseUrl manager aContext) = do
    token <- acsToken manager aContext
    reqInit <- parseUrl (baseUrl ++ "/" ++ qName ++ "/messages/head?timeout=" ++ (show timeout))
    res <-withSocketsDo $  httpLbs (reqInit { method = methodPost,
                              requestHeaders = [token]
                            }) manager
    return $ (getQLI res,responseBody res)
