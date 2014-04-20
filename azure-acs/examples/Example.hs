module Main where

import Webbaau.WindowsAzure.ACS 
import Network.HTTP.Conduit
import Network.HTTP.Client.Conduit
import Network.Connection (TLSSettings (..))
import qualified Data.ByteString.Char8 as C
import Control.Concurrent

main = do
  acsCtx <- acsContext (AcsInfo "-sb" (C.pack "http://blahblah.servicebus.windows.net/" ) (C.pack "owner") (C.pack "basjasj="))
  manager <- newManagerSettings (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  forkIO (acsToken manager acsCtx >>= print)
  t1 <- acsToken manager acsCtx
  print t1
  
                        
