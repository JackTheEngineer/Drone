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
import System.Console.GetOpt

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
  , compileIncludes :: [FilePath]
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


loadConfig filecontent = do
  cfg <- case Y.decodeEither' (BS.pack filecontent) of
           Right res -> return res
           Left err -> error $ show err
  let rpl = DT.replace
      rep a = unpck ((rpl (pck "<ccBase>") (pck (ccBase cfg))) (pck a))
      updated_config c = c { cc = rep (cc cfg)
                           , ccObjCopy = rep (ccObjCopy cfg)
                           , ccSize = rep (ccSize cfg)
                           , compileIncludes = map rep (compileIncludes cfg)
                           , sourceFiles = map rep (sourceFiles cfg)
                           , headerFiles = map rep (headerFiles cfg)}
  return $ updated_config cfg


data YamlConfig = MkYCFG String deriving Show

optDef :: Maybe String -> Either String YamlConfig
optDef s = case s of
              Nothing -> Left ""
              Just a -> Right $ MkYCFG a

argDef :: String -> Either String YamlConfig
argDef s = case s of
             "" -> Left "No argument passed"
             _ -> Right $ MkYCFG s

cmdFlags :: [OptDescr (Either String YamlConfig)]
cmdFlags = [ Option ['t'] ["test_sources"] (OptArg optDef "Yaml File") "YAML that defines test sources"
           , Option ['c'] ["compile_defines"] (ReqArg argDef "Yaml File") "Compilation Defines"
           ]

resultDir = "_build"
sourceGenDir = resultDir </> "sourceGen"

opt = shakeOptions{ shakeFiles=resultDir
                  , shakeThreads=8
                  , shakeReport = [resultDir </> "shake.trace"]
                  , shakeCreationCheck = False}

testSourcesLookups = [ "test/test_sources.yml"
                     , "bld/test_sources.yml"]
testCompilerDefinesLookups = [ "test/test_compiler.yml"
                             , "test/test_compilation_definition.yml"
                             , "test/test_compiler_define.yml"
                             , "bld/test_compiler.yml"
                             , "bld/test_compilation_definition.yml"
                             , "bld/test_compiler_define.yml"
                             ]

chooseCompilerDefine targets = do
  return $ targets !! 0

findTestSourcesYaml targets = do
  return $ targets !! 0


shakeIT :: IO()
shakeIT = shakeArgsWith opt cmdFlags $ \flags targets -> return $ Just $ do
  liftIO $ print flags
  liftIO $ print targets

  compilerConfig <- liftIO $ chooseCompilerDefine targets
  testSources <- liftIO $ findTestSourcesYaml targets
  
  want [(takeBaseName compilerConfig)]

  phony "clean" $ do
    putNormal ("Cleaning files in " ++ resultDir)
    removeFilesAfter resultDir ["//*"]

  phony "uC" $ do
    let elf_file = resultDir </> "result.elf"
        hex_file = elf_file -<.> "hex"
        bin_file = elf_file -<.> "bin"
    need [elf_file, hex_file, bin_file]

  yamlCfg <- newCache $ \f -> (readFile' f >>= loadConfig)

  resultDir <//> "*.hex" %> \out -> do
    let elf_file = out -<.> "elf"
    need [elf_file]
    c <- yamlCfg compilerConfig :: Action( Config )
    cmd_ (ccObjCopy c) "-O ihex" elf_file [out]

  resultDir <//> "*.bin" %> \out -> do
    let elf_file = out -<.> "elf"
    need [elf_file]
    c <- yamlCfg compilerConfig :: Action( Config )
    cmd_ (ccObjCopy c) "-O binary" elf_file [out]

  resultDir <//> "*.elf" %> \out -> do
    c <- yamlCfg compilerConfig :: Action( Config )
    allSources <- getDirectoryFiles "" (sourceFiles c)
    allHeaders <- getDirectoryFiles "" (headerFiles c)
    let os = [resultDir </> source -<.> "o" | source <- allSources]
        gcc = (cc c)
        map_file = out -<.> "map"
    need ((linkerFile c): os)
    cmd_ gcc "-o" [out] os ("-T" ++ (linkerFile c)) ("-Wl,-Map," ++ map_file) (ccLinkOptions c) (ccLinkLibs c)
    cmd_ (ccSize c) "--format=berkeley -x" [out]

  resultDir <//> "*.o" %> \out -> do
    c <- yamlCfg compilerConfig :: Action( Config )
    allSources <- getDirectoryFiles "" (sourceFiles c)
    allHeaders <- getDirectoryFiles "" (headerFiles c)
    sourceFile <- case ((takeDirectory out) == sourceGenDir) of
                    True -> return (out -<.> "c")
                    False ->findSourceWithFilterOrError out (sieveSourceOfObjFile out) allSources
    need [sourceFile]
    let uniqueDirs = reverse $ nub (map dropFileName (allHeaders ++ allSources))
        include_dirs = map (makeInclude . fromFilePath) uniqueDirs
        furtherCompileIncludes = map makeInclude (compileIncludes c)
        ending = takeExtension sourceFile
        m = out -<.> "m"
        asmOpts = intercalate " " ((ccAsmOptions c) ++  furtherCompileIncludes ++ include_dirs)
        cOpts = intercalate " " ((ccCompileOptions c) ++ furtherCompileIncludes ++ include_dirs)
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
    c <- yamlCfg compilerConfig :: Action( Config )
    allSources <- getDirectoryFiles "" (sourceFiles c)
    test_source <- findSourceWithFilterOrError out (sieveTestSourceOfRunner out) allSources
    cmd_ "ruby" genRunnerScript "-o" [out] "-t" runnerTemplate "-r" test_source

  "test_*" %> \out -> do
    let executable = resultDir </> out -<.> exe
    need [executable]
    cmd_ executable

  -- A rule that matches ("resultDir" <//> "test_*"), but not (resultDir <//> "*.o")
  (\file -> (((resultDir <//> "test_*" <.> exe) ?== file) && (not ((resultDir <//> "*.o") ?== file)))) ?> \out -> do
    c <- yamlCfg compilerConfig :: Action( Config )
    sources <- getDirectoryFiles "" (sourceFiles c)
    let runner_c = sourceGenDir </> (test_to_runner out) <.> "c"
        _objectFiles = [resultDir </> source -<.> "o" | source <- (sources)]
        objectFiles = ((runner_c -<.> "o"):_objectFiles)
        gcc = (cc c)
    need [runner_c]
    need objectFiles
    cmd_ gcc "-o" [out] objectFiles (ccLinkOptions c) (ccLinkLibs c)

main :: IO()
main = shakeIT 

