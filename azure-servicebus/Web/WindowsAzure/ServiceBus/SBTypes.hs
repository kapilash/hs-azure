-- |
-- Module : Web.WindowsAzure.ServiceBus.SBTypes
-- Description : Haskell Types for interacting with ServiceBus
-- Copyright : (c) Hemanth Kapila, 2014
-- License : BSD3
-- Maintainer : saihemanth@gmail.com
-- Stability  : Experimental
--
-- Provides Types used across the rest of the API.
-- 
{-# LANGUAGE OverloadedStrings #-}
module Web.WindowsAzure.ServiceBus.SBTypes 
   where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Int

import Web.WindowsAzure.ACS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit hiding (requestBodySource)
import Network.HTTP.Client.Conduit hiding (httpLbs)
import Network.HTTP.Types.Method (methodDelete, methodPost,methodPut)
import Network.HTTP.Types.Header
import Network(withSocketsDo)

import Network.Connection (TLSSettings (..))
import Data.Aeson
import Data.Monoid
import Control.Applicative
import qualified Data.CaseInsensitive as CI


-- | 'SBInfo' is encapsulation of Connection Information needed to connect to a Service Bus Namespace.
-- 
-- This information is typically found when you click on the /Connection Information/ link on the azure portal and comprises of
--
-- * ServiceBus namespace
-- * Issuer Name
-- * Issuer Key
-- 
data SBInfo = SBInfo String C.ByteString C.ByteString
              deriving (Show)

-- | Abstract type representing the service bus context.
data SBContext = SBContext String Manager AcsContext 

-- | a convenience function, where issuer name is owner
simpleSBInfo :: String -> String -> SBInfo
simpleSBInfo ns key = SBInfo ns (C.pack "owner") (C.pack key)

-- | Create SB Context from 'SBInfo'
sbContext :: SBInfo -> IO SBContext 
sbContext (SBInfo ns name key) = do
  aContext <- acsContext $ AcsInfo (ns ++ "-sb") (C.pack $ "http://" ++ ns ++ ".servicebus.windows.net") name key
  manager <- newManagerSettings (mkManagerSettings (TLSSettingsSimple True False False) Nothing) 
  return $ SBContext ("https://" ++ ns ++ ".servicebus.windows.net") manager aContext

-- | BrokerProperties
data BrokerProperties = BrokerProperties {
    deliveryCount :: Int,
    enqueuedSeqNumber :: Integer,
    enqueuedTimeUtc :: String,
    lockToken :: String,
    lockedUntilUtc :: String,
    bpMessageId :: String,
    bpSequenceNumber :: Integer,
    bpState :: String,
    bpTimeToLive :: Integer
}
 deriving (Show)

instance FromJSON BrokerProperties where
    parseJSON (Object v) = BrokerProperties <$>
                            v .: "DeliveryCount" <*>
                            v .: "EnqueuedSequenceNumber" <*>
                            v .: "EnqueuedTimeUtc" <*>
                            v .: "LockToken" <*>
                            v .: "LockedUntilUtc" <*>
                            v .: "MessageId" <*>
                            v .: "SequenceNumber" <*> 
                            v .: "State" <*>
                            v .: "TimeToLive"
    parseJSON _           = mempty

instance ToJSON BrokerProperties where
    toJSON (BrokerProperties dc esq etc lt luu msgId sn stt ttl) = object [
                                        "DeliveryCount" .= dc,
                                        "EnqueuedSequenceNumber" .= esq,
                                        "EnqueuedTimeUtc" .= etc,
                                        "LockToken" .= lt,
                                        "LockedUntilUtc" .= luu,
                                        "MessageId" .= msgId,
                                        "SequenceNumber" .= sn,
                                        "State" .= stt,
                                        "TimeToLive" .= ttl
                                        ]

emptyBP = BrokerProperties 0 0 "" "" "" "" 0 "" 0

-- | 'LockedMsgInfo' provides Information of the locked message.
--
data LockedMsgInfo = LockedMsgInfo String BrokerProperties 
                       deriving (Show)


-- | Unlock a messages that has been locked earlier.
-- 
-- see 'Web.WindowsAzure.ServiceBus.Topic.peekLockTopic' and 'Web.WindowsAzure.ServiceBus.Queue.peekLockQueue'.
--
-- Also consult <http://msdn.microsoft.com/en-us/library/hh780723.aspx Unlock message from Queue> and <http://msdn.microsoft.com/en-us/library/hh780737.aspx Unlock message from Subscription> for details on the underlying REST API.
unlockMessage :: LockedMsgInfo -> SBContext -> IO ()
unlockMessage (LockedMsgInfo url brokerProps) (SBContext baseUrl manager acsContext) = do
    token <- acsToken manager acsContext
    reqInit <- parseUrl url
    res <-withSocketsDo $  httpLbs (reqInit { method = methodPut,
                              requestHeaders = [token]
                            }) manager
    return ()

-- | Delete a message that has been locked earlier.
-- 
-- see 'Web.WindowsAzure.ServiceBus.Topic.peekLockTopic' and 'Web.WindowsAzure.ServiceBus.Queue.peekLockQueue'.
--
-- Also consult <http://msdn.microsoft.com/en-us/library/hh780767.aspx Delete Message from a Queue> and <http://msdn.microsoft.com/en-us/library/hh780768.aspx Delete Message from Subscription> for details on the underlying REST API.
deleteMessage :: LockedMsgInfo -> SBContext -> IO ()
deleteMessage (LockedMsgInfo url brokerProps) (SBContext baseUrl manager acsContext) = do
    token <- acsToken manager acsContext
    reqInit <- parseUrl url
    res <-withSocketsDo $  httpLbs (reqInit { method = methodDelete,
                              requestHeaders = [token]
                            }) manager
    return ()

-- | Renews lock on a locked message
-- 
-- see 'Web.WindowsAzure.ServiceBus.Topic.peekLockTopic' and 'Web.WindowsAzure.ServiceBus.Queue.peekLockQueue'.
--
-- Also consult <http://msdn.microsoft.com/en-us/library/jj839741.aspx Renew Lock for message from Queue> and <http://msdn.microsoft.com/en-us/library/jj839746.aspx Renew-Lock for a message from subscription> for details on the underlying REST API.
renewLock ::  LockedMsgInfo -> SBContext -> IO ()
renewLock (LockedMsgInfo url brokerProps) (SBContext baseUrl manager acsContext) = do
    token <- acsToken manager acsContext
    reqInit <- parseUrl url
    res <-withSocketsDo $  httpLbs (reqInit { method = methodPost,
                              requestHeaders = [token]
                            }) manager
    return ()



getQLI :: Response L.ByteString -> LockedMsgInfo
getQLI res = LockedMsgInfo loc bp 
    where
      loc = case lookup hLocation (responseHeaders res) of
            Nothing -> error "Expected Location Header in the response!"
            Just x  -> C.unpack x
      bp =   case lookup (CI.mk . C.pack $ "BrokerProperties") (responseHeaders res) of
                Nothing -> emptyBP
                Just bs -> case decode $ L.fromChunks [bs] of
                            Nothing -> emptyBP
                            Just b  -> b
