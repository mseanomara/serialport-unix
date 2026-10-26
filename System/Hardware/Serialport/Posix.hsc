{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE LambdaCase               #-}

-- |This module provides a Unix-only serial port interface.
--
-- > import qualified Data.ByteString.Char8 as B
-- > import System.Hardware.Serialport.Posix
-- > let port = "/dev/ttyUSB0"
-- > withSerial port defaultSerialSettings{ commSpeed = B2400 } $ \s -> do
-- >   send s $ B.pack "AT\r"
-- >   recv s 10 >>= print
--
-- Alternatively, use handles to perform IO:
--
-- > import System.IO
-- > import System.Hardware.Serialport.Posix
-- > let port = "/dev/ttyUSB0"
-- > hWithSerial port defaultSerialSettings $ \h -> do
-- >   hPutStr h "AT\r"
-- >   hGetLine h >>= print

module System.Hardware.Serialport.Posix (
  -- * Types
   BaudRate(..)
  ,StopBits(..)
  ,Parity(..)
  ,FlowControl(..)
  ,SerialPort
  -- * Configure port
  -- | You don't need the get or set functions, they are used by openSerial
  ,SerialPortSettings(..)
  ,defaultSerialSettings
  ,setSerialSettings
  ,getSerialSettings
  -- * Serial methods
  -- ** Device
  ,hOpenSerial
  ,openSerial
  ,closeSerial
  ,withSerial
  ,hWithSerial
  -- ** Sending & receiving
  ,send
  ,recv
  ,flush
  -- ** Line control
  ,setDTR
  ,setRTS
  ) where

import qualified Control.Exception as Ex
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Data.Either
import Data.Typeable
import Foreign (Ptr, nullPtr, castPtr, alloca, peek, with)
import Foreign.C
import GHC.IO.Handle
import GHC.IO.Encoding
import System.Hardware.Serialport.Types
import System.Posix.IO
import qualified System.Posix.IO.ByteString as BIO
import System.Posix.Types (Fd)

data SerialPort = SerialPort
  { fd           :: Fd
  , portSettings :: SerialPortSettings
  } deriving (Show, Typeable)

-- |Safer device function, so you don't forget to close the device
withSerial :: FilePath -> SerialPortSettings -> ( SerialPort -> IO a ) -> IO a
withSerial dev settings = Ex.bracket (openSerial dev settings) closeSerial

-- |Like `withSerial` but using `Handle`
hWithSerial :: FilePath -> SerialPortSettings -> (Handle -> IO a) -> IO a
hWithSerial dev settings = Ex.bracket (hOpenSerial dev settings) hClose

-- |Open and configure a serial port returning a standard Handle
hOpenSerial :: FilePath
           -> SerialPortSettings
           -> IO Handle
hOpenSerial dev settings = do
  h <- fdToHandle . fd =<< openSerial dev settings
  hSetBuffering h NoBuffering
  return h


-- |Open and configure a serial port
openSerial
  :: FilePath            -- ^ Serial port, such as @\/dev\/ttyS0@ or @\/dev\/ttyUSB0@
  -> SerialPortSettings
  -> IO SerialPort
openSerial dev settings = do
  fd' <- openFd dev ReadWrite defaultFileFlags { noctty = True, nonBlock = True }
  setTIOCEXCL fd'
  setFdOption fd' NonBlockingRead False
  let serial_port = SerialPort fd' defaultSerialSettings
  setSerialSettings serial_port settings


-- |Use specific encoding for an action and restore old encoding afterwards
withEncoding :: TextEncoding -> IO a -> IO a
#if MIN_VERSION_base(4,5,0)
withEncoding encoding fun = do
  cur_enc <- getForeignEncoding
  setForeignEncoding encoding
  result <- fun
  setForeignEncoding cur_enc
  return result
#else
withEncoding _ fun = fun
#endif


-- |Receive bytes, given the maximum number
recv :: SerialPort -> Int -> IO B.ByteString
recv port n = do
  result <- withEncoding char8 $ Ex.try $ BIO.fdRead (fd port) count :: IO (Either IOError B.ByteString)
  return $ fromRight B.empty result
  where
    count = fromIntegral n

-- |Send bytes
send
  :: SerialPort
  -> B.ByteString
  -> IO Int          -- ^ Number of bytes actually sent
send port msg =
  fromIntegral <$> withEncoding char8 (fdWrite (fd port) (B.unpack msg))


-- |Flush buffers
flush :: SerialPort -> IO ()
flush port = discardData (fd port) BothQueues


-- |Close the serial port
closeSerial :: SerialPort -> IO ()
closeSerial = closeFd . fd


#include <sys/ioctl.h>

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

cIoctl' :: Fd -> Int -> Ptr d -> IO ()
cIoctl' f req =
  throwErrnoIfMinus1_ "ioctl" .
     c_ioctl (fromIntegral f) (fromIntegral req) . castPtr


getTIOCM :: Fd -> IO Int
getTIOCM fd' =
  alloca $ \p -> cIoctl' fd' #{const TIOCMGET} p >> peek p


setTIOCM :: Fd -> Int -> IO ()
setTIOCM fd' val =
  with val $ cIoctl' fd' #{const TIOCMSET}


setTIOCEXCL :: Fd -> IO ()
setTIOCEXCL fd' = cIoctl' fd' #{const TIOCEXCL} nullPtr


-- |Set the Data Terminal Ready level
setDTR :: SerialPort -> Bool -> IO ()
setDTR (SerialPort fd' _) set = do
  current <- getTIOCM fd'
  setTIOCM fd' $ if set
                   then current .|. #{const TIOCM_DTR}
                   else current .&. complement #{const TIOCM_DTR}


-- |Set the Ready to send level
setRTS :: SerialPort -> Bool -> IO ()
setRTS (SerialPort fd' _) set = do
  current <- getTIOCM fd'
  setTIOCM fd' $ if set
                   then current .|. #{const TIOCM_RTS}
                   else current .&. complement #{const TIOCM_RTS}


-- |Configure the serial port
setSerialSettings :: SerialPort           -- ^ The currently opened serial port
                  -> SerialPortSettings   -- ^ The new settings
                  -> IO SerialPort        -- ^ New serial port
setSerialSettings port new_settings = do
  termOpts <- getTerminalAttributes $ fd port
  let termOpts' = configureSettings termOpts new_settings
  setTerminalAttributes (fd port) termOpts' Immediately
  return $ SerialPort (fd port) new_settings


-- |Get configuration from serial port
getSerialSettings :: SerialPort -> SerialPortSettings
getSerialSettings = portSettings


withParity :: TerminalAttributes -> Parity -> TerminalAttributes
withParity termOpts Even =
    termOpts `withMode` EnableParity
             `withoutMode` OddParity
withParity termOpts Odd =
    termOpts `withMode` EnableParity
             `withMode` OddParity
withParity termOpts NoParity =
    termOpts `withoutMode` EnableParity


withFlowControl :: TerminalAttributes -> FlowControl -> TerminalAttributes
withFlowControl termOpts NoFlowControl =
    termOpts `withoutMode` StartStopInput
             `withoutMode` StartStopOutput
withFlowControl termOpts Software =
    termOpts `withMode` StartStopInput
             `withMode` StartStopOutput


withStopBits :: TerminalAttributes -> StopBits -> TerminalAttributes
withStopBits termOpts One =
    termOpts `withoutMode` TwoStopBits
withStopBits termOpts Two =
    termOpts `withMode` TwoStopBits


configureSettings :: TerminalAttributes -> SerialPortSettings -> TerminalAttributes
configureSettings termOpts settings =
    termOpts `withInputSpeed` commSpeed settings
             `withOutputSpeed` commSpeed settings
             `withBits` fromIntegral (bitsPerWord settings)
             `withStopBits` stopb settings
             `withParity` parity settings
             `withFlowControl` flowControl settings
             `withoutMode` EnableEcho
             `withoutMode` EchoErase
             `withoutMode` EchoKill
             `withoutMode` ProcessInput
             `withoutMode` ProcessOutput
             `withoutMode` MapCRtoLF
             `withoutMode` EchoLF
             `withoutMode` HangupOnClose
             `withoutMode` KeyboardInterrupts
             `withoutMode` ExtendedFunctions
             `withMode` LocalMode
             `withMode` ReadEnable
             `withTime` timeout settings
             `withMinInput` 0
