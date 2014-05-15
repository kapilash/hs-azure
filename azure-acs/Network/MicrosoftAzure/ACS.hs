-- |
-- Module : Network.MicrosoftAzure.ACS
-- Description : API for requesting password token from Windows Azure ACS.
-- Copyright : (c) Hemanth Kapila, 2014
-- License : BSD3
-- Maintainer : saihemanth@gmail.com
-- Stability  : Experimental
--
-- Provides API to request password token from WindowsAzure ACS. The security token is needed by web applications and services that handle authentication using ACS.
-- Please refer to <http://msdn.microsoft.com/en-us/library/hh278947.aspx ACS Management Service API> for further information on ACS.
--
-- Following piece of code illustrates the use of API
--
-- @
-- import Network.MicrosoftAzure.ACS
-- import Network.HTTP.Conduit
-- import Network.HTTP.Client.Conduit
-- import Network.Connection (TLSSettings (..))
-- import qualified Data.ByteString.Char8 as C
--
-- main = do
--      acsCtx <- acsContext (AcsInfo "blahblah-sb" (C.pack "http://blahblah.servicebus.windows.net/" ) (C.pack "owner") (C.pack "blahBlahBlahBlah"))
--      manager <- newManagerSettings (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
--      t1 <- acsToken manager acsCtx
--      print t1
-- @
--
module Network.MicrosoftAzure.ACS (
  -- * Types
  -- * Acs Info
  AcsInfo (..),
  -- * Acs Context
  AcsContext,
  AcsToken,
  -- * functions
  acsContext,
  acsToken
  )where

import Network.HTTP.Conduit
import Data.Conduit
import Network.HTTP.Types.URI
import qualified Data.ByteString.Char8 as C
import qualified Data.Attoparsec.ByteString.Lazy as AP
import Data.Attoparsec.ByteString.Char8
import Control.Concurrent.MVar
import Network.HTTP.Types.Method(methodPost)
import Network.HTTP.Types.Header
import Data.Time.Clock(getCurrentTime, addUTCTime,UTCTime)
import Data.Conduit.Attoparsec(sinkParser)
import Data.Conduit.Binary(sourceLbs)
import Network(withSocketsDo)

-- | 'AcsInfo' encapsulates the information needed to get the token from Azure ACS:
--
--
--     *  acs namespace
--     *  the relying party address
--     *  the issuer and
--     *  the issuer key.
data AcsInfo = AcsInfo String !C.ByteString !C.ByteString !C.ByteString
                 deriving (Eq,Show)

{- | synonym for the ACS password token
-}
type AcsToken = Header

data AcsResponse = AcsResponse !AcsToken !UTCTime
                   | NotConnectedToAcs


{- | An abstract datatype that keeps track of acs token expiry and gets a new one as necessary.
-}
data AcsContext = AcsContext  !AcsInfo (MVar AcsResponse)



-- | construct the context object. This call does not perform any network call yet.
acsContext :: AcsInfo -> IO AcsContext
acsContext a = do
  b <- newMVar NotConnectedToAcs
  return $ AcsContext a b


{-# INLINE canReuse #-}
canReuse :: UTCTime -> AcsResponse -> Bool
canReuse _ NotConnectedToAcs = False
canReuse currentTime (AcsResponse _ time) = currentTime < time

{-# INLINE wrapToken #-}
wrapToken :: AcsResponse -> AcsToken
wrapToken (AcsResponse t _) = t
wrapToken _ = undefined

-- | If a valid token is available, it is returned. Otherwise, requests password a fresh password token from  windows azure acs
--
--  Refer to <http://www.yesodweb.com/book/http-conduit  HTTP Conduit> for information about creating and using 'Manager'
acsToken :: Manager -> AcsContext -> IO AcsToken
acsToken manager (AcsContext info mv) = do
  utcTime <- getCurrentTime
  acsResp <- takeMVar mv
  if canReuse utcTime acsResp
     then do { putMVar mv acsResp; return (wrapToken acsResp)}
    else do
    resp <- doAcsPost info manager
    putMVar mv resp
    return (wrapToken resp)



doAcsPost :: AcsInfo -> Manager  ->IO AcsResponse
doAcsPost (AcsInfo url endpoint issuer key) manager = do
  request' <- parseUrl ("https://" ++ url ++ ".accesscontrol.windows.net/WRAPv0.9/")
  let request = addBody  $ request' {
             method = methodPost
           }
  res <- withSocketsDo $  httpLbs request manager
  utcTime <- getCurrentTime
  acsResp <- sourceLbs (responseBody res) $$ (sinkParser $ parseResponse utcTime)
  return acsResp
 where
   addBody = urlEncodedBody [(C.pack "wrap_scope",endpoint),(C.pack "wrap_name", issuer),(C.pack "wrap_password",key)]
   parseResponse currTime = do
     AP.takeTill (== 61)
     AP.anyWord8
     b1 <- AP.takeTill (== 38)
     AP.anyWord8
     AP.takeTill (== 61)
     AP.anyWord8
     i <- decimal
     return $ AcsResponse (toHeader b1) (addUTCTime (fromInteger $ i - 300) currTime)
   toHeader bs = (hAuthorization, C.concat [(C.pack  "WRAP access_token=\""), (urlDecode False bs), C.pack "\""])
