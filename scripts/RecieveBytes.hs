 {-# LANGUAGE OverloadedStrings          #-}
import System.IO
import System.Hardware.Serialport
import Data.Binary.Get as G
import Data.Binary.Strict.BitGet as BG
import Control.Monad
import Data.Word
import Data.ByteString as B
import Data.ByteString.Lazy.Internal as BLI
import Data.ByteString.Lazy as BL
import Data.Binary.IEEE754 
-- import Prelude.Math

-- data StatusCode = StatusCode { txFifoFull :: Word8
--                              , rxPipeNum :: Word8
--                              , maxRetransmits :: Word8
--                              , txDataSent :: Word8
--                              , rxDataReady :: Word8
--                              , rBank :: Word8 } deriving Show

data StatusCode = StatusCode { rBank :: Word8
                             , rxDataReady :: Word8
                             , txDataSent :: Word8
                             , maxRetransmits :: Word8
                             , rxPipeNum :: Word8
                             , txFifoFull :: Word8 } deriving Show
statusCode = StatusCode <$>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 3 <*>
             BG.getAsWord8 1

data FifoStatus = FifoStatus { _r2 :: Word8
                             , tx_reuse :: Word8
                             , tx_full :: Word8
                             , tx_empty :: Word8
                             , _r1 :: Word8
                             , rx_full :: Word8
                             , rx_empty :: Word8 } deriving Show
fifoStatus = FifoStatus <$>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 2 <*>
             BG.getAsWord8 1 <*>
             BG.getAsWord8 1

data ObserveTx = ObserveTx { packetLostCNT :: Word8
                           , autoRetransmissionCNT :: Word8} deriving Show
observeTx = ObserveTx <$> BG.getAsWord8 4 <*> BG.getAsWord8 4

parseBinary :: Get [Word16]
parseBinary = do
  replicateM 4 G.getWord16le

parseFloats :: Get [Float]
parseFloats = do
  q1 <- getFloat32le
  q2 <- getFloat32le
  q3 <- getFloat32le
  q4 <- getFloat32le
  let psi = atan2 (2*q2*q3 - 2*q1*q4) (2*q1*q1 + 2*q2*q2 - 1)
      theta = -1 * (asin (2*q2*q4 - 2*q1*q3))
      phi = atan2 (2*q3*q4 - 2*q1*q2) (2*q1*q1 * 2*q4*q4 - 1)
  return [psi, theta, phi]

parseRFM75_StatusCode = do
  status <- G.getByteString 1
  otx <- G.getByteString 1
  fifo <- G.getByteString 1
  msglen <- G.getWord8
  let s = BG.runBitGet status statusCode
      f = BG.runBitGet fifo fifoStatus
      o = BG.runBitGet otx observeTx
  return (msglen, f, o, s)

first (a, _, _, _) = a
scnd (_, b, _, _) = b
thrd (_, _, c, _) = c
fourth (_, _, _, d) = d

getAndPrint serport = do
  bytes <- B.hGetLine serport
  if (B.length bytes) == 32 then do
    print $ G.runGet parseFloats $ fromStrict bytes
  else return ()
  -- let rfm = G.runGet parseRFM75_StatusCode $ fromStrict bytes
  -- print $ first rfm
  -- print $ scnd rfm
  -- print $ thrd rfm
  -- print $ fourth rfm
  
receiveByteAndPrint :: IO ()
receiveByteAndPrint = do
  serport <- openUart "/dev/ttyACM0"
  replicateM 10000 (getAndPrint serport)
  hClose serport
  
openUart :: String -> IO(Handle)
openUart port = do
  hOpenSerial port defaultSerialSettings {commSpeed = CS115200
                                         , timeout = 3000}
main :: IO()
main = do
  receiveByteAndPrint  
