 {-# LANGUAGE OverloadedStrings          #-}
import Data.Text as T
import Data.Text.IO as TIO
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

modifytestContent file = do
  filecontent <- TIO.readFile file
  TIO.writeFile file (T.replace "- src//" "- Drone_src//" filecontent)

main :: IO()
main = do
  test_yamls <- getDirectoryFilesIO "" ["test/test_*.yml"]
  mapM_ modifytestContent test_yamls

