import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as DT
import System.Environment
import System.Process


data Config = Config {
   sourceFiles :: [FilePattern]
  , headerFiles :: [FilePattern]
  , ccBase :: FilePath
  , cc :: FilePath
  , ccObjCopy :: FilePath
  , ccSize :: FilePath
  , ccAsmOptions :: [String]
  , ccCompileOptions :: [String]
  , ccLinkOptions :: [String]
  , ccLinkLibs :: [String]
  , linkerFile :: String
} deriving Show

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
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
    v .: pck "linkerFile"
  parseJSON _ = fail "Expected Object for Config value"

pck = DT.pack
unpck = DT.unpack

fromFilePath :: FilePath -> String
fromFilePath xs = xs

makeInclude = ((++) "-I ")

runner_to_test r = "test" ++  (((drop 4) . takeBaseName) r)
test_to_runner t = "run_" ++ (((drop 4) . takeBaseName) t)

dropPreDir preDir file = let l = length(splitPath preDir) in 
                           foldr1 (</>) (drop l (splitDirectories file))

sieveSourceOfObjFile result_Dir o_file p  = ((dropExtension . (dropPreDir result_Dir)) o_file) == (dropExtension p)
sieveTestSourceOfRunner r p = (runner_to_test r) == (takeBaseName p)
sieveForYamlFile r p = (takeBaseName r) == (takeBaseName p)
  
findSourceWithFilterOrError ::  FilePath -> (FilePath -> FilePath -> Bool) -> [FilePath] -> Action(FilePath)
findSourceWithFilterOrError key sieve allSources = do
  let res = filter (sieve key) allSources
  case res of
    (test_source:[]) -> return test_source
    _                -> liftIO $ do
                          mapM_ print allSources
                          error ("In the above sources, there was no match, or too many: " ++ key)

(-#>) = findSourceWithFilterOrError 

loadConfig filecontent = do
  cfg <- case Y.decodeEither' (BS.pack filecontent) of
           Right res -> return res
           Left err -> error $ show err
  let rpl = DT.replace
      rep a = unpck ((rpl (pck "<ccBase>") (pck (ccBase cfg))) (pck a))
      updated_config c = c { cc = rep (cc cfg)
                           , ccObjCopy = rep (ccObjCopy cfg)
                           , ccSize = rep (ccSize cfg)
                           , sourceFiles = map rep (sourceFiles cfg)
                           , headerFiles = map rep (headerFiles cfg)}
  return $ updated_config cfg

resultDir = "_build"
opt = shakeOptions{ shakeFiles=resultDir
                  , shakeThreads=8
                  , shakeReport = [resultDir </> "shakereport.html"]
                  , shakeCreationCheck = False}

flags = []


-- Only with 'shakeArgsWith', there is no automatic "withoutActions"
-- "withoutActions" makes Rules do nothing, and build the File,
-- which was specified at the commandlineb.
-- (with "withoutActions") If the file already exists,
-- like with the yamlConfig in our case, it would do nothing
shakeIT :: IO()
shakeIT = shakeArgsWith opt flags $ \options args -> return $ Just $ do

  let target = (args !! 0)
  case ("//*.ucbuild" ?== target ) of
    True -> do -- Building for microcontroller
      let buildname = takeBaseName target
          elf_file = resultDir </> buildname </> buildname <.> "elf"
          hex_file = elf_file -<.> "hex"
          bin_file = elf_file -<.> "bin"
      want [elf_file, hex_file, bin_file]
    False -> do -- Building a test
      want [takeBaseName target]

  test_yamls <- liftIO $ getDirectoryFilesIO "" ["test/test_*.yml"] 

  let allConfigs = case elem target test_yamls of
                     True -> test_yamls
                     False -> (target:test_yamls)
                
  phony "clean" $ do
    putNormal ("Cleaning files in " ++ resultDir)
    removeFilesAfter resultDir ["//*"]

  phony "tests" $ do
    need $ (map takeBaseName test_yamls)

  yamlCfg <- newCache $ \f -> (readFile' f >>= loadConfig)
  getFiles <- newCache $ \f -> do
    liftIO $ getDirectoryFilesIO "" f

--- Helper Functions ---  
  let configNameFromSource s = (splitDirectories s) !! 1 
  let configFromSource s = do
        let configname = configNameFromSource s
        conf <- (-#>) configname sieveForYamlFile allConfigs
        yamlCfg conf
        
  let _objDir s = resultDir </> configNameFromSource s
  let _sourceGenDir s = resultDir </> configNameFromSource s </> "sourceGen"
--- Helper Functions ---
  
  resultDir <//> "*.hex" %> \out -> do
    let elf_file = out -<.> "elf"
    need [elf_file]
    c <- configFromSource out :: Action( Config )
    cmd_ (ccObjCopy c) "-O ihex" elf_file [out]

  resultDir <//> "*.bin" %> \out -> do
    let elf_file = out -<.> "elf"
    need [elf_file]
    c <- configFromSource out :: Action( Config )
    cmd_ (ccObjCopy c) "-O binary" elf_file [out]

  resultDir <//> "*.elf" %> \out -> do
    c <- configFromSource out :: Action( Config )
    allSources <- getFiles (sourceFiles c)
    allHeaders <- getFiles (headerFiles c)
    let os = [(_objDir out) </> source -<.> "o" | source <- allSources]
        gcc = (cc c)
        map_file = out -<.> "map"
    need ((linkerFile c): os)
    cmd_ gcc "-o" [out] os ("-T" ++ (linkerFile c)) ("-Wl,-Map," ++ map_file) (ccLinkOptions c) (ccLinkLibs c)
    cmd_ (ccSize c) "--format=berkeley -x" [out]

  resultDir <//> "*.o" %> \out -> do
    c <- configFromSource out :: Action( Config )
    allSources <- getDirectoryFiles "" (sourceFiles c)
    allHeaders <- getDirectoryFiles "" (headerFiles c)
    sourceFile <- case ((takeDirectory out) == (_sourceGenDir out)) of
                    True -> return (out -<.> "c")
                    False -> (-#>) out (sieveSourceOfObjFile (_objDir out)) allSources
    need [sourceFile]
    let uniqueDirs = reverse $ nub (map dropFileName (allHeaders ++ allSources))
        include_dirs = map (makeInclude . fromFilePath) uniqueDirs
        ending = takeExtension sourceFile
        m = out -<.> "m"
        asmOpts = intercalate " " ((ccAsmOptions c) ++ include_dirs)
        cOpts = intercalate " " ((ccCompileOptions c) ++ include_dirs)
        gcc = (cc c)
        asm = cmd_ gcc "-MMD -MF" [m] asmOpts sourceFile "-o" [out]
        cCmd = cmd_ gcc sourceFile "-MMD -MF" [m] cOpts "-o" [out]
    case ending of
      ".c" -> do
        cCmd
      ".S" -> do
        asm
      ".asm" -> do
        asm
      _ -> error ("Not A Valid FileEnding " ++ show ending)
    neededMakefileDependencies m

  resultDir <//> "run_*.c" %> \out -> do
    let genRunnerScript = "bld" </> "generate_test_runner.rb"
        runnerTemplate =  "bld" </> "runner_template.c"
    c <- configFromSource out :: Action( Config )
    allSources <- getDirectoryFiles "" (sourceFiles c)
    test_source <- (-#>) out sieveTestSourceOfRunner allSources
    cmd_ "ruby" genRunnerScript "-o" [out] "-t" runnerTemplate "-r" test_source

  "test_*" %> \out -> do
    let executable = resultDir </> (takeBaseName out) </> out -<.> exe
    need [executable]
    cmd_ executable

  -- A rule that matches ("resultDir" <//> "test_*"), but not (resultDir <//> "*.o")
  (\f -> (((resultDir <//> "test_*" <.> exe) ?== f) && not ((resultDir <//> "*.o") ?== f))) ?> \out -> do
    c <- configFromSource out :: Action( Config )
    sources <- getDirectoryFiles "" (sourceFiles c)
    let sourceGenDir = _sourceGenDir out
        runner_c = sourceGenDir </> (test_to_runner out) <.> "c"
        _objectFiles = [(_objDir out) </> source -<.> "o" | source <- (sources)]
        objectFiles = ((runner_c -<.> "o"):_objectFiles)
        gcc = (cc c)
    need [runner_c]
    need objectFiles
    cmd_ gcc "-o" [out] objectFiles (ccLinkOptions c) (ccLinkLibs c)

main :: IO()
main = shakeIT 
