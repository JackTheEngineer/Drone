 {-# LANGUAGE OverloadedStrings          #-}
import System.IO
import System.Hardware.Serialport
import Data.Binary.Get as BG
import Control.Monad
import Data.Word
import Data.ByteString.Lazy.Internal as BLI
import Data.ByteString.Lazy as BL


parseBinary :: Get [Word16]
parseBinary = do
  replicateM 4 getWord16le

getAndPrint serport = do
  BL.hGet serport 9 >>= (\l -> print (BG.runGet parseBinary l))
  
receiveByteAndPrint :: IO ()
receiveByteAndPrint = do
  serport <- openUart "/dev/ttyUSB0"
  replicateM_ 500 (getAndPrint serport)
  hClose serport
  
openUart :: String -> IO(Handle)
openUart port = do
  hOpenSerial port defaultSerialSettings {commSpeed = CS115200
                                         , timeout = 1}
  
main :: IO()
main = do
  receiveByteAndPrint  
