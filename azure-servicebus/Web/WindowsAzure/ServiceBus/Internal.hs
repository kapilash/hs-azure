module Web.WindowsAzure.ServiceBus.Internal where


import Web.WindowsAzure.ACS
import Web.WindowsAzure.ServiceBus.SBTypes
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Conduit hiding (requestBodySource)
import Network.HTTP.Client.Conduit 
import Network.Connection (TLSSettings (..))
