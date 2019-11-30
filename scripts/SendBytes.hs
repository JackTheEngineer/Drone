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
import Data.ByteString.Builder.Extra
import Data.ByteString.Lazy.Internal as BLI
import Data.ByteString.Lazy as BL
import Data.Binary.IEEE754 

openUart :: String -> IO(Handle)
openUart port = do
  hOpenSerial port defaultSerialSettings {commSpeed = CS115200
                                         , timeout = 3000}

genBs =  mconcat [ BB.word8  1
                 , floatHost 180.0  -- Kp 
                 , floatHost 0.09    -- Kd 
                 , BB.word8 $ 10 ]

  
bangSerPort serport = do
  BB.hPutBuilder serport genBs
  threadDelay (20*1000) -- 
  
main :: IO ()
main = do
  serport <- openUart "/dev/ttyACM0"
  hSetBuffering serport $ BlockBuffering (Just 10) -- NoBuffering --
  bangSerPort serport 
  threadDelay (300*1000)
  hClose serport
