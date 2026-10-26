{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wno-orphans            #-}
{-# OPTIONS_HADDOCK hide                #-}
module System.Hardware.Serialport.Types
  ( module System.Posix.Terminal
  , StopBits(..)
  , Parity(..)
  , FlowControl(..)
  , SerialPortSettings(..)
  , defaultSerialSettings
  ) where

import Data.Word
import System.Posix.Terminal

deriving newtype instance Read BaudRate

data StopBits = One | Two
  deriving (Show, Read, Eq, Bounded)

data Parity = Even | Odd | NoParity
  deriving (Show, Read, Eq)

data FlowControl = Software | NoFlowControl
  deriving (Show, Read, Eq)

data SerialPortSettings = SerialPortSettings
  { commSpeed   :: BaudRate,    -- ^ baudrate
    bitsPerWord :: Word8,       -- ^ Number of bits in a word
    stopb       :: StopBits,    -- ^ Number of stop bits
    parity      :: Parity,      -- ^ Type of parity
    flowControl :: FlowControl, -- ^ Type of flowcontrol
    timeout     :: Int          -- ^ Timeout when receiving a char in tenth of seconds
  } deriving (Show, Read, Eq)


-- | Most commonly used configuration
--
--  - 9600 baud
--
--  - 8 data bits
--
--  - 1 stop bit
--
--  - no parity
--
--  - no flow control
--
--  - 0.1 second receive timeout
--
defaultSerialSettings :: SerialPortSettings
defaultSerialSettings =
  SerialPortSettings B9600 8 One NoParity NoFlowControl 1
