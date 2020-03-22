 {-# LANGUAGE OverloadedStrings          #-}
import Data.Text as T
import Data.Text.IO as TIO
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

modifytestContent file = do
  filecontent <- TIO.readFile file
  -- TIO.writeFile file (T.replace "- src//" "- Drone_src//" filecontent)
  TIO.writeFile file (T.replace "/home/jakov/bin/gcc-arm-none-eabi-4_9-2015q3" "/home/jakov/bin/gcc-arm-none-eabi-4_9-2014q4" filecontent)
  

main :: IO()
main = do
  -- test_yamls <- getDirectoryFilesIO "" ["test/test_*.yml"]
  test_yamls <- getDirectoryFilesIO "" ["bld/*.ucbuild"]
  mapM_ modifytestContent test_yamls

