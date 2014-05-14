-- |
-- Module : Web.WindowsAzure.ServiceBus.Topic
-- Description : API for reading from and writing to ServiceBus Topic
-- Copyright : (c) Hemanth Kapila, 2014
-- License : BSD3
-- Maintainer : saihemanth@gmail.com
-- Stability  : Experimental
--
-- Provides API to pull from and push to ServiceBus topic
-- Please refer to <http://msdn.microsoft.com/en-us/library/hh780752.aspx Service Bus Rest API> for information on the API provided by
-- Microsoft Service bus.
--
-- Simple example for how to use this library is as below
--
-- @
-- import Web.WindowsAzure.ServiceBus.Topic
-- import Web.WindowsAzure.ServiceBus
-- import qualified Data.ByteString.Char8 as C
-- 
-- topicName = "topicName"
-- subscriptionName = "subscriptionName"
-- sbNamespace = "namespace"
-- sbIssuerKey = C.pack "1287361251262as="
-- sbIssuerName = C.pack "owner"
-- 
-- sbinfo = SBInfo sbNamespace sbIssuerName sbIssuerKey 
-- message = C.pack "Hello from Haskell"
-- 
-- main = do
--   sbContext <- sbContext sbinfo
--   sendTopicBS topicName message sbContext
--   res <- destructiveRead topicName subscriptionName 30 sbContext
--   print res
-- @
-- 
-- see examples (available as a part of distribution) for a more detailed example.

module Web.WindowsAzure.ServiceBus.Topic( 
  -- * Pushing data to Topic
  sendTopicBS,
  sendTopicLBS,
  sendTopicBodySrc,
  -- * Reading data from Topic
  destructiveRead,
  peekLockTopic
   )where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Int

import Web.WindowsAzure.ACS
import Web.WindowsAzure.ServiceBus.SBTypes
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit hiding (requestBodySource)
import Network.HTTP.Client.Conduit hiding (httpLbs)
import Network.HTTP.Types.Method (methodDelete, methodPost,methodPut)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method

import qualified Data.CaseInsensitive as CI
import Network.Connection (TLSSettings (..))
import Data.Aeson
import Network(withSocketsDo)

-- | Internal low-level method for performing HTTP calls. 
-- 
-- Not exposed to the user
sendTopicRequest :: String -> RequestBody -> SBContext -> IO ()
sendTopicRequest topicName body (SBContext baseUrl manager aContext)  = do
  token <- acsToken manager aContext
  reqInit <- parseUrl (baseUrl ++ "/" ++ topicName ++ "/messages")
  withSocketsDo $ httpLbs (reqInit { method = methodPost,
                     requestHeaders = [token],
                     requestBody = body
                   }) manager
  return ()

-- | Internal low-level method for creating the HTTP calls. For internal use. 
--  
-- Not exposed to the user
destructiveReadRequest :: String -> String -> Int -> SBContext -> IO L.ByteString
destructiveReadRequest topic subsc timeout (SBContext baseUrl manager aContext) = do
  token <- acsToken manager aContext
  reqInit <- parseUrl (baseUrl ++ "/" ++ topic ++ "/Subscriptions/" ++ subsc ++ "/messages/head?timeout=" ++ (show timeout))
  res <-withSocketsDo $  httpLbs ( reqInit { method = methodDelete, 
                     requestHeaders = [token]
                   }) manager
  return $ responseBody res

-- | publish a message containing 'C.ByteString' to a topic.
--
-- The following publishes a strict bytestring \bs\ to topic \t\
--
-- @
-- sendTopicBS t bs ctx
-- @
sendTopicBS ::  String -> C.ByteString -> SBContext -> IO ()
sendTopicBS   topicName content context = 
  sendTopicRequest  topicName (RequestBodyBS content) context
  
  
-- | publish a message containing 'L.ByteString' to a topic
--  
--  The following publishes a lazy bytestring ,\lbs\, to topic \t\,  
--  
-- @  
-- sendTopicLBS t lbs  ctx
-- @  
--  
sendTopicLBS :: String -> L.ByteString -> SBContext -> IO ()  
sendTopicLBS  topicName content context = 
  sendTopicRequest  topicName (RequestBodyLBS content) context
  
  
-- | publish from a 'Source' (refer to 'requestBodySource')
sendTopicBodySrc ::  String -> Int64 -> Source IO C.ByteString -> SBContext -> IO ()
sendTopicBodySrc  topicName  len bodysrc context =   sendTopicRequest topicName (requestBodySource len bodysrc) context

  
-- | Reads and deletes the message from a topic at a given subscription. 
--  
--  
-- In order to destructively read the latest message from the subscription /subsc/ on topic /t/  (with a time out of n seconds),   
--  
--  
-- @  
-- destructiveRead t subsc n context
-- @
--  Note that the timeout can be at the most 55 seconds. 
destructiveRead :: String -> String -> Int -> SBContext -> IO (L.ByteString)
destructiveRead  topic subsc timeout context =  destructiveReadRequest  topic subsc (timeout `mod` 55) context

-- | Peek Lock Message from a Topic. Non-Destructive Read.
--
-- Atomically retrieves the message from a topic (on a given subscription)  without deleting it. The message is locked for a duration so that it is not visible to other
-- receivers.
--
-- Refer <http://msdn.microsoft.com/en-us/library/hh780722.aspx ServiceBus documentation> for semantics of the underlying REST API.
peekLockTopic :: String -> String -> Int -> SBContext -> IO (LockedMsgInfo,L.ByteString)
peekLockTopic topic subscr timeout (SBContext baseUrl manager aContext) = do
    token <- acsToken manager aContext
    reqInit <- parseUrl (baseUrl ++ "/" ++ topic ++ "/Subscriptions/" ++ subscr ++ "/messages/head?timeout=" ++ (show timeout))
    res <-withSocketsDo $  httpLbs (reqInit { method = methodPost,
                              requestHeaders = [token]
                            }) manager
    return $ (getQLI res,responseBody res)
