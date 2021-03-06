-- |
-- Module : Web.WindowsAzure.ServiceBus
-- Description : Haskell Types for interacting with ServiceBus
-- Copyright : (c) Hemanth Kapila, 2014
-- License : BSD3
-- Maintainer : saihemanth@gmail.com
-- Stability  : Experimental
--
-- __deprecated__ Use "Network.MicrosoftAzure.ServiceBus" instead.
--
-- Haskell API for working with <http://azure.microsoft.com/en-us/services/messaging/ Microsoft Azure ServiceBus>

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
