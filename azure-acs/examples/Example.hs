module Main where

import Web.WindowsAzure.ACS 
import Network.HTTP.Conduit
import Network.HTTP.Client.Conduit
import Network.Connection (TLSSettings (..))
import qualified Data.ByteString.Char8 as C
import Control.Concurrent


acsNS = "XXXX"
issuerKey = C.pack "YYYYYYYYYYYYYYYYYYYYYYY=="
issuerName = C.pack "owner"
relyingPartyUrl = C.pack "http://XXXX.servicebus.windows.net" 

main = do
  acsCtx <- acsContext (AcsInfo acsNS relyingPartyUrl issuerName issuerKey)
  manager <- newManagerSettings (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  forkIO (acsToken manager acsCtx >>= print)
  t1 <- acsToken manager acsCtx
  print t1
  
                        
