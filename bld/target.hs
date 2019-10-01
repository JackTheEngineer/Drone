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

pck = DT.pack
unpck = DT.unpack

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

makeInclude = ((++) "-I ")

sourceOfObjectFile :: FilePath -> [FilePath] -> Action(FilePath)
sourceOfObjectFile o_file allSources = do
  -- If you want to make an arbitrarily deep results directory,
  -- change this func  :>>----------------------------<<:, and pass it the result directory as a parameter
  let pathNoExtension = (dropExtension . dropDirectory1) o_file
      compare = (\cp p -> (dropExtension p) == cp)
      res = filter (compare pathNoExtension) allSources in
    do 
      case res of
        (x:[]) -> return x
        _ -> error "The UNIMAGINABLE happened. The function 'sourceOfObjectFile' did not work"

shakeIT :: Config -> IO()
shakeIT c = shakeArgs shakeOptions{shakeFiles=(resultsDir c)
                                  ,shakeThreads=6} $ do
  let resultDir = resultsDir c
      elf_file = resultDir </> "result.elf"
      hex_file = elf_file -<.> "hex"
      bin_file = elf_file -<.> "bin"
      map_file = elf_file -<.> "map"
      
  allSources <- liftIO $ getDirectoryFilesIO "" (sourceFiles c)
  allHeaders <- liftIO $ getDirectoryFilesIO "" (headerFiles c)

  let uniqueDirs = nub (map dropFileName (allHeaders ++ allSources))
      include_dirs = map (makeInclude . fromFilePath) uniqueDirs
      furtherCompileIncludes = map makeInclude (compileIncludes c)
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
    sourceFile <- sourceOfObjectFile out allSources
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

loadConfig :: String -> IO(Config)
loadConfig filename = do
  file <- BS.readFile filename
  cfg <- Y.decodeThrow file :: IO(Config)
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

