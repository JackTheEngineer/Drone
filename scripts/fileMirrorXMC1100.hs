{-# LANGUAGE OverloadedStrings #-}

import System.FSNotify
import System.Directory
import Control.Applicative((<$>))
import Control.Exception(throw)
import Control.Monad(when,forM_,forever)
import System.FilePath ((</>))
import Development.Shake.FilePath (splitPath
                                  , splitDirectories
                                  , takeDirectory)
import Control.Concurrent (threadDelay)
import Data.String
import Data.Time


dropPreDir preDir file = let l = length(splitPath preDir) in foldr1 (</>) (drop l (splitDirectories file))

(<<>>) path source dest = dest </> dropPreDir source path

copy :: FilePath -> FilePath -> FilePath -> IO()
copy source dest path = do
  let targetFile = dest </> dropPreDir source path
  print $ "Copying from: " ++ show path ++ "to: " ++ show targetFile
  createDirectoryIfMissing True $ takeDirectory targetFile
  copyFile path targetFile

remove :: FilePath -> FilePath -> FilePath -> IO()
remove source dest path = do
  let d = (<<>>) path source dest
  print $ "Removing: " ++ show d
  removeFile d

rm :: FilePath -> FilePath -> FilePath -> IO()
rm source dest path = do
  let newFileName = (<<>>) path source dest
  print $ "RM: " ++ newFileName
  removePathForcibly newFileName

cpL :: FilePath -> FilePath -> FilePath -> IO()
cpL sourcedir destdir path = do
  print $ "CP: " ++ path
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then do
      let newDirName = (<<>>) path sourcedir destdir
      print $ "Creating Directory: " ++ newDirName
      createDirectory newDirName
    else copy sourcedir destdir path

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  whenM (not <$> doesDirectoryExist src) $
    throw (userError "source does not exist")
  dstExists <- doesDirectoryExist dst

  if dstExists
  then do
    removePathForcibly dst
    createDirectory dst
  else createDirectory dst

  content <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) content
  forM_ xs $ \name -> do
    let srcPath = src </> name
    let destPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then do
        print $ "Copying dir from: " ++ srcPath ++ " to: " ++ destPath
        copyDir srcPath destPath
      else do
        print $ "Copying File from: " ++ srcPath ++ " to: " ++ destPath
        copyFile srcPath destPath

  where
    doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
    orM xs = or <$> sequence xs
    whenM s r = s >>= flip when r

copyModifyingDirs :: FilePath -> FilePath -> Event -> IO()
copyModifyingDirs sourcedir destination ev =
  case ev of
    Added path _ _ -> cpL sourcedir destination path
    Modified path _ _ -> cpL sourcedir destination path
    Removed path _ _ -> rm sourcedir destination path
    Unknown {} -> error "Unknown Event !!"


main :: IO()
main = do
  let sourcedir = "/home/jakov/vmshare/XMC1100/Generated" :: String
      destdir = "/home/jakov/programming/Drone/hardware/XMC1100New/Generated" :: String

  -- copyDir sourcedir destdir
  
  -- withManager $ \mgr -> do
  --   -- start a watching job (in the background)
  --   watchTree
  --     mgr          -- manager
  --     sourcedir    -- directory to watch
  --     (const True) -- predicate
  --     (copyModifyingDirs sourcedir destdir)

  let wconf = WatchConfig { confDebounce = Debounce (realToFrac 4.0)
                          , confPollInterval = 4000000
                          , confUsePolling = True}

  withManagerConf wconf $ \mgr -> do
    -- start a watching job (in the background)
    watchTree
      mgr          -- manager
      sourcedir    -- directory to watch
      (const True) -- predicate
      (copyModifyingDirs sourcedir destdir)

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000



