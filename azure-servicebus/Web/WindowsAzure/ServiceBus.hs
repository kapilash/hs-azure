module Web.WindowsAzure.ServiceBus (
    -- * Servicebus Types
    SBInfo (..),
    -- | Abstract type representing the service bus context.
    SBContext,
    -- Broker Properties 
    BrokerProperties (..),
    -- * Initialization
    simpleSBInfo,
    sbContext,
    -- * Locked Messages
    unlockMessage,
    renewLock,
    deleteMessage
    ) where

import Web.WindowsAzure.ServiceBus.SBTypes
