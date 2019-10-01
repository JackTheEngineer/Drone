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

-- Attention ! The build system relies on the results directory
-- being exactly 1 directory deep.
-- You can change the name of the result directory.
-- BUT f.e "shakebuild/something" is not allowed.
-- If you need it to be deep, you have to
-- update the dule on building '.o' files,
-- replacing the first 'n' elements of the
-- 'source' filepath with nothing,
-- where 'n' is the depth of the resultsDir.
-- currently dropDirectory1 is used.
resultsDir = "shakebuild" 

arm_base = "/home/jakov/bin/gcc-arm-none-eabi-4_9-2014q4/bin" 
gcc = arm_base </> "arm-none-eabi-gcc"
objcopy = arm_base </> "arm-none-eabi-objcopy"
sizeCmd = arm_base </> "arm-none-eabi-size"

armAsmOptions = [ "-x assembler-with-cpp"
                , "-c"
                , "-DXMC4700_F144x2048"
                , "-Wall"
                , "-mfloat-abi=softfp"
                , "-fmessage-length=0"
                , "-mcpu=cortex-m4"
                , "-mfpu=fpv4-sp-d16"
                , "-mthumb"
                , "-g3"
                , "-gdwarf-2" ]
armCompileOptions = [ "-c"
                    , "-DXMC4700_F144x2048"
                    , "-O0"
                    , "-ffunction-sections"
                    , "-fdata-sections"
                    , "-Wall"
                    , "-std=gnu99"
                    , "-mfloat-abi=softfp"
                    , "-pipe"
                    , "-fmessage-length=0"
                    , "-mcpu=cortex-m4"
                    , "-mfpu=fpv4-sp-d16"
                    , "-mthumb"
                    , "-g3"
                    , "-gdwarf-2" ] 
armLinkOptions = [ "-nostartfiles"
                 , "-Xlinker"
                 , "--gc-sections"
                 , "-specs=nano.specs"
                 , "-specs=nosys.specs"
                 , "-mfloat-abi=softfp"
                 , "-mfpu=fpv4-sp-d16"
                 , "-mcpu=cortex-m4"
                 , "-mthumb"
                 , "-g3"
                 , "-gdwarf-2"] :: [String]
linkedLibraries = [ "-lm" ] :: [String]

makeInclude = ((++) "-I ")
-- At the command line they end up like:
-- '-I "compile-include-path"', f.e '-I "PATH_TO_ARM_BASE/arm-none-eabi/include"'
-- where ' are the string literals
-- please make sure to add a '/' literal at the end
furtherCompileIncludes = map makeInclude
                           [arm_base </> "arm-none-eabi/include/"
                           , "vendor/Libraries/XMCLib/inc/"
                           , "vendor/Libraries/CMSIS/Include/"
                           , "vendor/Libraries/CMSIS/Infineon/XMC4700_series/Include/"
                           ]
linker_file = "linker_script.ld"
header_files = ["src//*.h"]
source_files = ["src//*.c"
               , "vendor/Libraries/XMCLib/src//*.c"
               , "vendor/Libraries/Newlib//*.c"
               , "Startup//*.c"
               , "Startup//*.S"]

sourceOfObjectFile :: FilePath -> [FilePath] -> Action(FilePath)
sourceOfObjectFile o_file allSources = do
  let pathNoExtension = (dropExtension . dropDirectory1) o_file
      compare = (\cp p -> (dropExtension p) == cp)
      res = filter (compare pathNoExtension) allSources in
    do 
      case res of
        (x:[]) -> return x
        _ -> error "The UNIMAGINABLE happened. The function 'sourceOfObjectFile' did not work"

data Config = Config {
  ccBase :: FilePath
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

pa = DT.pack

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .: pa "ccBase" <*>
    v .: pa "cc" <*>
    v .: pa "ccObjCopy" <*>
    v .: pa "ccSize" <*>
    v .: pa "sourceFiles" <*>
    v .: pa "headerFiles" <*>
    v .: pa "ccAsmOptions" <*>
    v .: pa "ccCompileOptions" <*>
    v .: pa "ccLinkOptions" <*>
    v .: pa "ccLinkLibs" <*>
    v .: pa "compileIncludes" <*>
    v .: pa "linkerFile"
  parseJSON _ = fail "Expected Object for Config value"


shakeIT :: Config -> IO()
shakeIT c = shakeArgs shakeOptions{shakeFiles=resultsDir
                                  ,shakeThreads=6} $ do
  let elf_file = resultsDir </> "result.elf"
      hex_file = elf_file -<.> "hex"
      bin_file = elf_file -<.> "bin"
      map_file = elf_file -<.> "map"
      
  want [elf_file, hex_file, bin_file]
  allSources <- liftIO $ getDirectoryFilesIO "" (sourceFiles c)
  allHeaders <- liftIO $ getDirectoryFilesIO "" (headerFiles c)
  let uniqueDirs = nub (map dropFileName (allHeaders ++ allSources))

  phony "clean" $ do
    putNormal ("Cleaning files in " ++ resultsDir)
    removeFilesAfter resultsDir ["src//*", "/*.*"]

  bin_file %> \out -> do
    need [elf_file]
    cmd_ objcopy "-O ihex" elf_file [out]

  hex_file %> \out -> do
    need [elf_file]
    cmd_ objcopy "-O binary" elf_file [out]
    
  elf_file %> \out -> do
    let os = [resultsDir </> c -<.> "o" | c <- allSources]
    need (linker_file : os) 
    cmd_ gcc "-o" [out] os ("-T" ++ linker_file) ("-Wl,-Map," ++ map_file) armLinkOptions linkedLibraries

  resultsDir <//> "*.o" %> \out -> do
    sourceFile <- sourceOfObjectFile out allSources
    need [sourceFile]
    let ending = takeExtension sourceFile
        m = out -<.> "m"
        include_dirs = map (makeInclude . fromFilePath) uniqueDirs
        asmOpts = intercalate " " (armAsmOptions ++  furtherCompileIncludes ++ include_dirs)
        cOpts = intercalate " " (armCompileOptions ++ furtherCompileIncludes ++ include_dirs)
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
      _ -> error "Not A Valid FileEnding"
  

main = do
  args <- getArgs
  print args
  file <- BS.readFile (args !! 0) 
  config <- Y.decodeThrow file :: IO(Config)
  shakeIT config
    


fromFilePath :: FilePath -> String
fromFilePath xs = xs

