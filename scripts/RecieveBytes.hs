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
  print bytes
  let rfm = G.runGet parseRFM75_StatusCode $ fromStrict bytes
  print $ first rfm
  print $ scnd rfm
  print $ thrd rfm
  print $ fourth rfm
  
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
