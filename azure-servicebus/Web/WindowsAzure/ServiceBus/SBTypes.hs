-- |
-- Module : Web.WindowsAzure.ServiceBus.SBTypes
-- Description : Defines Types for dealing with Windows Azure Service Bus
-- Copyright : (c) Hemanth Kapila, 2014
-- License : BSD3
-- Maintainer : saihemanth@gmail.com
-- Stability  : Experimental
--
-- 

module Web.WindowsAzure.ServiceBus.SBTypes( 
  -- * Types
  SBInfo(..),
  SBContext,
  -- * initialization
  sbContext,
  simpleSBInfo,
  -- * low-level functions for creating 'Request'
  enQueueRequest,
  deQueueRequest
   )where

import Web.WindowsAzure.ACS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit hiding (requestBodySource)
import Network.HTTP.Client.Conduit hiding (httpLbs)
import Network.HTTP.Types.Method(methodPost,methodDelete)

import Network.Connection (TLSSettings (..))



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


-- | Internal low-level method for performing HTTP calls. 
-- 
-- should be avoided by most users.
enQueueRequest :: String -> RequestBody -> SBContext -> IO ()
enQueueRequest queueName body (SBContext baseUrl manager aContext)  = do
  token <- acsToken manager aContext
  reqInit <- parseUrl (baseUrl ++ "/" ++ queueName ++ "/messages")
  httpLbs (reqInit { method = methodPost,
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
  res <- httpLbs ( reqInit { method = methodDelete, 
                     requestHeaders = [token]
                   }) manager
  return $ responseBody res
