-- |
-- Module : Network.MicrosoftAzure.ServiceBus
-- Description : Haskell Types for interacting with ServiceBus
-- Copyright : (c) Hemanth Kapila, 2014
-- License : BSD3
-- Maintainer : saihemanth@gmail.com
-- Stability  : Experimental
--
-- Haskell API for working with <http://azure.microsoft.com/en-us/services/messaging/ Microsoft Azure ServiceBus>
module Network.MicrosoftAzure.ServiceBus (
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

import Network.MicrosoftAzure.ServiceBus.SBTypes
