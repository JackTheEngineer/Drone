import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Data.List
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as DT
import System.Environment

data Config = Config {
  resultsDir :: FilePath
  , ccBase :: FilePath
  , cc :: FilePath
  , ccObjCopy :: FilePath
  , ccSize :: FilePath
  , sourceFiles :: [FilePattern]
  , headerFiles :: [FilePattern]
  , ccAsmOptions :: [String]
  , ccCompileOptions :: [String]
  , ccLinkOptions :: [String]
  , ccLinkLibs :: [String]
  , compileIncludes :: [FilePath]
  , linkerFile :: String
} deriving Show

pck = DT.pack
unpck = DT.unpack

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .: pck "ccBase" <*>
    v .: pck "cc" <*>
    v .: pck "ccObjCopy" <*>
    v .: pck "ccSize" <*>
    v .: pck "sourceFiles" <*>
    v .: pck "headerFiles" <*>
    v .: pck "ccAsmOptions" <*>
    v .: pck "ccCompileOptions" <*>
    v .: pck "ccLinkOptions" <*>
    v .: pck "ccLinkLibs" <*>
    v .: pck "compileIncludes" <*>
    v .: pck "linkerFile" <*>
    v .: pck "resultsDir"
  parseJSON _ = fail "Expected Object for Config value"

makeInclude = ((++) "-I ")

sourceOfObjectFile :: FilePath -> [FilePath] -> Action(FilePath)
sourceOfObjectFile o_file allSources = do
  -- If you want to make an arbitrarily deep results directory,
  -- change this func  :>>----------------------------<<:
  let pathNoExtension = (dropExtension . dropDirectory1) o_file
      compare = (\cp p -> (dropExtension p) == cp)
      res = filter (compare pathNoExtension) allSources in
    do 
      case res of
        (x:[]) -> return x
        _ -> error "The UNIMAGINABLE happened. The function 'sourceOfObjectFile' did not work"


startsWith :: String -> String -> Bool
startsWith key wholestring =
  (take (length key) wholestring) == key

-- should support targets "test" -> runs all available tests and test_files
-- and "test_something"
-- and "test_something"
shakeIT :: Config -> IO()
shakeIT c = shakeArgs shakeOptions{shakeFiles=(resultsDir c)
                                  ,shakeThreads=6} $ do

  allSources <- liftIO $ getDirectoryFilesIO "" (sourceFiles c)
  allHeaders <- liftIO $ getDirectoryFilesIO "" (headerFiles c)
  
  let resultDir = resultsDir c
      sourceGenDir = resultDir </> "sourceGen"
      runnername t = (((drop 4) . takeBaseName)) t
      -- The Order of allHeaders and allSources Matters,
      -- as the directories that are shown first with "-I", are being searched first
      uniqueDirs = reverse $ nub (map dropFileName (allHeaders ++ allSources))
      include_dirs = map (makeInclude . fromFilePath) uniqueDirs
      furtherCompileIncludes = map makeInclude (compileIncludes c)
      gcc = (cc c)

  resultDir <//> "run_*.c" %> \out -> do
    let genRunnerScript = "bld" </> "generate_test_runner.rb"
        runnerTemplate =  "bld" </> "runner_template.c"
        compare = (\cp p -> (takeBaseName p) == cp)
        test_source = (filter (compare ("test" ++ (runnername out))) allSources) !! 0
    need [sourceGenDir]
    cmd_ "ruby" genRunnerScript "-o" [out] "-t" runnerTemplate "-r" test_source

  "test_*" %> \out -> do
    let executable = resultDir </> out -<.> exe
    need [executable]
    cmd_ executable
    
  resultDir <//> "test_*" <.> exe %> \out -> do
    sources <- getDirectoryFiles "" (sourceFiles c)
    let runner_c = sourceGenDir </> ("run_" ++ (runnername out)) <.> "c"
        os = [resultDir </> source -<.> "o" | source <- (runner_c:sources)]
    need [runner_c]
    need os
    cmd_ gcc "-o" [out] os (ccLinkOptions c) (ccLinkLibs c)

  resultDir <//> "*.o" %> \out -> do
    sourceFile <- case ((takeDirectory out) == sourceGenDir) of
                     True -> return (out -<.> "c")
                     False -> sourceOfObjectFile out allSources
    let m = out -<.> "m"
        cOpts = intercalate " " ((ccCompileOptions c) ++ furtherCompileIncludes ++ include_dirs)
    cmd_ gcc sourceFile "-MMD -MF" [m] cOpts "-o" [out]
    
    neededMakefileDependencies m
    

loadConfig :: String -> IO(Config)
loadConfig filename = do
  file <- BS.readFile filename
  cfg <- Y.decodeThrow file :: IO(Config)
  print cfg
  let rpl = DT.replace
      rep a = unpck ((rpl (pck "<ccBase>") (pck (ccBase cfg))) (pck a))
      updated_config c = c { cc = rep (cc cfg)
                           , ccObjCopy = rep (ccObjCopy cfg)
                           , ccSize = rep (ccSize cfg)
                           , compileIncludes = map rep (compileIncludes cfg)
                           , sourceFiles = map rep (sourceFiles cfg)
                           , headerFiles = map rep (headerFiles cfg)}
      newcfg = updated_config cfg
  return newcfg

main = do
  args <- getArgs
  if (length(args) < 2) then
    do
      print("Usage: stack runghc Shakebuild.hs <target> configYaml.yml")
    else
    do
      newcfg <- loadConfig (args !! 1)
      shakeIT newcfg

fromFilePath :: FilePath -> String
fromFilePath xs = xs

