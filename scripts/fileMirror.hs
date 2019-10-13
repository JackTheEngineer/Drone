{-# LANGUAGE OverloadedStrings #-}

import System.FSNotify
import System.Directory
import Control.Applicative((<$>))
import Control.Exception(throw)
import Control.Monad(when,forM_)
import System.FilePath ((</>))
import Development.Shake.FilePath (splitPath, splitDirectories)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)


dropPreDir preDir file = let l = length(splitPath preDir) in foldr1 (</>) (drop l (splitDirectories file))

(<<>>) path source dest = dest </> (dropPreDir source path)


copy :: FilePath -> FilePath -> FilePath -> IO()
copy source dest path = do
  let d = dest </> (dropPreDir source path)
  print $ "Copying from: " ++ show path ++ "to: " ++ show d
  copyFile path d

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


cp sourcedir destdir path = do
  print $ "CP: " ++ path
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then do
    let newDirName = (<<>>) path sourcedir destdir
    print $ "Createing Directory: " ++ newDirName
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
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then do
		print $ "Copying dir from: " ++ srcPath ++ " to: " ++ dstPath
		copyDir srcPath dstPath
		
      else do
		print $ "Copying File from: " ++ srcPath ++ " to: " ++ dstPath
		copyFile srcPath dstPath

  where
    doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
    orM xs = or <$> sequence xs
    whenM s r = s >>= flip when r

copyModifyingDirs :: FilePath -> FilePath -> Event -> IO()
copyModifyingDirs sourcedir destination ev = do
  case ev of
    Added path _ _ -> cp sourcedir destination path
    Modified path _ _ -> cp sourcedir destination path
    Removed path _ _ -> rm sourcedir destination path
    Unknown _ _ _ -> error "Unknown Event !!"


main :: IO()
main = do
  let sourcedir = "/home/jakov/vmshare/XMC4500/Generated"
      destdir = "/home/jakov/programming/Drone/hardware/XMC4500Dave/Generated"
  copyDir sourcedir destdir
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    watchTree
      mgr          -- manager
      sourcedir     -- directory to watch
      (const True) -- predicate
      (copyModifyingDirs sourcedir destdir)

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
