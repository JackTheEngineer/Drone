import System.IO
import System.Hardware.Serialport
import Data.Binary.Get as G
import Data.Binary.Put
import Data.Binary.Strict.BitGet as BG

import Control.Monad
import Control.Concurrent

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Lazy.Internal as BLI
import Data.ByteString.Lazy as BL
import Data.Binary.IEEE754 

openUart :: String -> IO(Handle)
openUart port = do
  hOpenSerial port defaultSerialSettings {commSpeed = CS115200
                                         , timeout = 3000}

-- genBs = do
--   putWord8 1
--   putWord8 50   kp
--   putWord8 100  ki
--   putWord8 150  kd
--   putWord8 10

genBs = mconcat [ BB.word8 1  
                , BB.word8 50 
                , BB.word8 100
                , BB.word8 150
                , BB.word8 10 ]

  
bangSerPort serport builder =
  replicateM_ 1 $ do
    BB.hPutBuilder serport builder
    threadDelay (5*1000) -- 
  
main :: IO ()
main = do
  serport <- openUart "/dev/ttyACM0"
  hSetBuffering serport $ BlockBuffering (Just 5)
  bangSerPort serport genBs
  threadDelay (100*1000)
  hClose serport
