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
import System.Process

data Config = Config {
  resultsDir :: FilePath
  , sourceFiles :: [FilePattern]
  , headerFiles :: [FilePattern]
  , ccBase :: FilePath
  , cc :: FilePath
  , ccObjCopy :: FilePath
  , ccSize :: FilePath
  , ccAsmOptions :: [String]
  , ccCompileOptions :: [String]
  , ccLinkOptions :: [String]
  , ccLinkLibs :: [String]
  , compileIncludes :: [FilePath]
  , linkerFile :: String
} deriving Show

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .: pck "resultsDir" <*>
    v .: pck "sourceFiles" <*>
    v .: pck "headerFiles" <*>
    v .: pck "ccBase" <*>
    v .: pck "cc" <*>
    v .: pck "ccObjCopy" <*>
    v .: pck "ccSize" <*>
    v .: pck "ccAsmOptions" <*>
    v .: pck "ccCompileOptions" <*>
    v .: pck "ccLinkOptions" <*>
    v .: pck "ccLinkLibs" <*>
    v .: pck "compileIncludes" <*>
    v .: pck "linkerFile"
  parseJSON _ = fail "Expected Object for Config value"

pck = DT.pack
unpck = DT.unpack

fromFilePath :: FilePath -> String
fromFilePath xs = xs

makeInclude = ((++) "-I ")

runner_to_test r = "test" ++  (((drop 4) . takeBaseName) r)
test_to_runner t = "run_" ++ (((drop 4) . takeBaseName) t)

  -- If you want to make an arbitrarily deep results directory,
  -- change this func underneath the funny symbols  :>>----------------------------<<:,
  -- and pass it the result directory as a parameter
sieveSourceOfObjFile o_file p  = ((dropExtension . dropDirectory1) o_file) == (dropExtension p)

sieveTestSourceOfRunner r p = (runner_to_test r) == (takeBaseName p)
  
findSourceWithFilterOrError ::  FilePath -> (FilePath -> Bool) -> [FilePath] -> Action(FilePath)
findSourceWithFilterOrError key sieve allSources = do
  let res = filter sieve allSources
  case res of
    (test_source:[]) -> return test_source
    _                -> liftIO $ do
                          mapM_ print allSources
                          error ("In the above sources, there was no match, or too many: " ++ key)

shakeIT :: Config -> IO()
shakeIT c = shakeArgs shakeOptions{ shakeFiles=(resultsDir c)
                                  , shakeThreads=6
                                  , shakeReport = ["shake.trace"]
                                  , shakeCreationCheck = False} $ do
  let resultDir = resultsDir c
      elf_file = resultDir </> "result.elf"
      hex_file = elf_file -<.> "hex"
      bin_file = elf_file -<.> "bin"
      map_file = elf_file -<.> "map"
      
  allSources <- liftIO $ getDirectoryFilesIO "" (sourceFiles c)
  allHeaders <- liftIO $ getDirectoryFilesIO "" (headerFiles c)

  let uniqueDirs = reverse $ nub (map dropFileName (allHeaders ++ allSources))
      include_dirs = map (makeInclude . fromFilePath) uniqueDirs
      furtherCompileIncludes = map makeInclude (compileIncludes c)
      sourceGenDir = resultDir </> "sourceGen"
      gcc = (cc c)

  phony "arm" $ do
    need [elf_file, hex_file, bin_file]
  
  phony "clean" $ do
    putNormal ("Cleaning files in " ++ resultDir)
    removeFilesAfter resultDir ["//*"]

  bin_file %> \out -> do
    need [elf_file]
    cmd_ (ccObjCopy c) "-O ihex" elf_file [out]

  hex_file %> \out -> do
    need [elf_file]
    cmd_ (ccObjCopy c) "-O binary" elf_file [out]
    
  elf_file %> \out -> do
    let os = [resultDir </> source -<.> "o" | source <- allSources]
    need ((linkerFile c): os)
    cmd_ gcc "-o" [out] os ("-T" ++ (linkerFile c)) ("-Wl,-Map," ++ map_file) (ccLinkOptions c) (ccLinkLibs c)
    cmd_ (ccSize c) "--format=berkeley -x" [out]

  resultDir <//> "*.o" %> \out -> do
    sourceFile <- case ((takeDirectory out) == sourceGenDir) of
                    True -> return (out -<.> "c")
                    False ->findSourceWithFilterOrError out (sieveSourceOfObjFile out) allSources
    need [sourceFile]
    let ending = takeExtension sourceFile
        m = out -<.> "m"
        asmOpts = intercalate " " ((ccAsmOptions c) ++  furtherCompileIncludes ++ include_dirs)
        cOpts = intercalate " " ((ccCompileOptions c) ++ furtherCompileIncludes ++ include_dirs)
        asm = cmd_ gcc "-MMD -MF" [m] asmOpts sourceFile "-o" [out]
        cCmd = cmd_ gcc sourceFile "-MMD -MF" [m] cOpts "-o" [out]
    case ending of
      ".c" -> do
        cCmd
        neededMakefileDependencies m
      ".S" -> do
        asm
        neededMakefileDependencies m
      ".asm" -> do
        asm
        neededMakefileDependencies m
      _ -> error ("Not A Valid FileEnding " ++ show ending)

  resultDir <//> "run_*.c" %> \out -> do
    let genRunnerScript = "bld" </> "generate_test_runner.rb"
        runnerTemplate =  "bld" </> "runner_template.c"
    test_source <- findSourceWithFilterOrError out (sieveTestSourceOfRunner out) allSources
    cmd_ "ruby" genRunnerScript "-o" [out] "-t" runnerTemplate "-r" test_source

  "test_*" %> \out -> do
    let executable = resultDir </> out -<.> ".exe"
    need [executable]
    cmd_ executable
    
  resultDir <//> "test_*" <.> "exe" %> \out -> do
    sources <- getDirectoryFiles "" (sourceFiles c)
    let runner_c = sourceGenDir </> (test_to_runner out) <.> "c"
        _os = [resultDir </> source -<.> "o" | source <- (sources)]
        os = ((runner_c -<.> "o"):_os)
    need [runner_c]
    need os
    cmd_ gcc "-o" [out] os (ccLinkOptions c) (ccLinkLibs c)
      

loadConfig :: String -> IO(Config)
loadConfig filename = do
  cfg <- Y.decodeFileThrow filename :: IO(Config)
  let rpl = DT.replace
      rep a = unpck ((rpl (pck "<ccBase>") (pck (ccBase cfg))) (pck a))
      updated_config c = c { cc = rep (cc cfg)
                           , ccObjCopy = rep (ccObjCopy cfg)
                           , ccSize = rep (ccSize cfg)
                           , compileIncludes = map rep (compileIncludes cfg)
                           , sourceFiles = map rep (sourceFiles cfg)
                           , headerFiles = map rep (headerFiles cfg)}
  return $ updated_config cfg

runAllTests :: IO()
runAllTests = do
  yamls <- getDirectoryFilesIO "" ["test" </> "test_*.yml"]
  let testnames = map takeBaseName yamls
      zipped = zip testnames yamls
      command (t, y) = "stack runghc bld/YACBS.hs " ++ t ++ " " ++ y
      commands = map command zipped
  --mapM_ print commands
  mapM_ callCommand commands

checkTests :: String -> IO()
checkTests arg = do
  case arg of
    "tests" -> runAllTests
    _ -> printUsage


-- Counting arguments diables usage of flags :(
main :: IO()
main = do
  args <- getArgs
  case length(args) of 
    1 -> checkTests (args !! 0)
    2 -> (loadConfig (args !! 1)) >>= shakeIT
    _ -> printUsage

printUsage :: IO()
printUsage = do  
  print("For one target, f.e:")
  print("    # stack runghc bld/YACBS.hs arm bld/arm.yml")
  print("Or, if you want to run all tests execute: ")
  print("    # stack runghc bld/YACBS.hs tests")

