import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Control.Monad
import Text.Parsec.Char hiding (newline)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char (digit)

import Data.List
import Data.List.Split
import Data.Maybe (fromMaybe)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as DT
import System.Environment
import System.Process

--   +-+-+-+-+-+-+-+-+ +-+-+-+-+ +-+-+-+-+-+-+ 
--   |G|e|n|e|r|a|t|e| |T|E|S|T| |R|u|n|n|e|r| 
--   +-+-+-+-+-+-+-+-+ +-+-+-+-+ +-+-+-+-+-+-+
lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           many space
           return x
cIdentifier = many1 (satisfy (\char -> (char >= 'A' &&
                                         char <= 'Z') ||
                                        (char >= 'a' &&
                                         char <= 'z') ||
                                        (char >= '0' &&
                                         char <= '9') ||
                                        (char == '_')))
skipUntilEOL = do
  skipMany (satisfy (\c -> (c /= '\n') && (c /= '\r')))
  endOfLine

tP :: Parser (Maybe (String, String))
tP = do
  many space
  lexeme (try (string "IGNORE_TEST") <|> string ("TEST"))
  lexeme $ char '('
  group <- lexeme cIdentifier
  lexeme $ char ','
  name <- lexeme cIdentifier
  lexeme $ char ')'
  skipUntilEOL
  return $ Just (group, name)

readGroupsAndNames :: Parser [(String, String)]
readGroupsAndNames = do
  res <- many (try tP <|> (skipUntilEOL >> return Nothing))
  return [m | Just m <- res]

genGroupRunner :: [(String, String)] -> String
genGroupRunner groupBlock =
  let gn = fst (head groupBlock)
      runs = concatMap (\(g, f) ->  "    RUN_TEST_CASE("++g++","++f++");\n") groupBlock in
    "TEST_GROUP_RUNNER("++gn++"){\n" ++ runs ++ "}\n\n";

genRunAllGroups :: [String] -> String
genRunAllGroups groups =
  let runs = concatMap (\g -> "    RUN_TEST_GROUP("++g++");\n") groups in
    "static void run_all_tests(void){\n" ++ runs ++ "}\n"

genRunAllTests groupsAndNames =
  let groups = nub $ map fst groupsAndNames
      filterFuncs = map (\g e -> filter (\e -> fst e == g) groupsAndNames) groups
      groupBlocks = map ($ groupsAndNames) filterFuncs
      groupRunners = concatMap genGroupRunner groupBlocks
      allRunner = genRunAllGroups groups in
    groupRunners ++ allRunner

generateRunner :: FilePath -> FilePath -> FilePath -> Action()
generateRunner testSource runnerTemplate resultname = do
  t <- readFile' testSource
  rt <- readFile' runnerTemplate
  let e_groupsAndNames =  parse readGroupsAndNames "(unknown)" t
  groupsAndNames <- case e_groupsAndNames of
                      Right res -> return res
                      Left err -> error $ show err
  let runAllTestsCode = genRunAllTests groupsAndNames
      blocks = splitOn "//CONTENT" rt
  runnerCode <- case length blocks of
                  2 -> return (head blocks ++ runAllTestsCode ++ blocks !! 1)
                  _ -> error "There was an error with splitting the runner Template on the keyword '//CONTENT'"
  writeFile' resultname runnerCode

--   +-+-+-+ +-+-+-+-+-+-+-+-+ +-+-+-+-+ +-+-+-+-+-+-+
--   |E|N|D| |G|e|n|e|r|a|t|e| |T|e|s|t| |R|u|n|n|e|r|
--   +-+-+-+ +-+-+-+-+-+-+-+-+ +-+-+-+-+ +-+-+-+-+-+-+

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

makeInclude = (++) "-I "

runnerToTest r = "test" ++  (drop 4 . takeBaseName) r

testToRunner t = "run_" ++ (drop 4 . takeBaseName) t

dropPreDir preDir file = let l = length(splitPath preDir) in
                           foldr1 (</>) (drop l (splitDirectories file))

sieveSourceOfObjFile result_Dir o_file p  = (dropExtension . (dropPreDir result_Dir)) o_file == dropExtension p
sieveTestSourceOfRunner r p = runnerToTest r == takeBaseName p
sieveForYamlFile r p = takeBaseName r == takeBaseName p

findSourceWithFilterOrError ::  FilePath -> (FilePath -> FilePath -> Bool) -> [FilePath] -> Action FilePath
findSourceWithFilterOrError key sieve allSources = do
  let res = filter (sieve key) allSources
  case res of
    [test_source] -> return test_source
    _                -> liftIO $ do
                          mapM_ print allSources
                          error ("In the above sources, there was no match, or too many: " ++ key)

infix 2 -#>
(-#>) = findSourceWithFilterOrError

loadConfig filecontent = do
  cfg <- case Y.decodeEither' (BS.pack filecontent) of
           Right res -> return res
           Left err -> error $ show err
  let rpl = DT.replace
      rep a = unpck (rpl (pck "<ccBase>") (pck (ccBase cfg)) (pck a))
      updated_config c = c { cc = rep (cc cfg)
                           , ccObjCopy = rep (ccObjCopy cfg)
                           , ccSize = rep (ccSize cfg)
                           , sourceFiles = map rep (sourceFiles cfg)
                           , headerFiles = map rep (headerFiles cfg)}
  return $ updated_config cfg

resultDir = "_build"

uC_Triple buildname = let
    elf_file = resultDir </> buildname </> buildname <.> "elf"
    hex_file = elf_file -<.> "hex"
    bin_file = elf_file -<.> "bin" in
    [elf_file, hex_file, bin_file]


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

  let target = head args
  if "//*.ucbuild" ?== target
    then (do -- Building for microcontroller
             let buildname = takeBaseName target
             want $ uC_Triple buildname)
    else (
             want [takeBaseName target])

  test_yamls <- liftIO $ getDirectoryFilesIO "" ["test/test_*.yml"]

  let allConfigs = if target `elem` test_yamls
        then test_yamls
        else target:test_yamls

  phony "clean" $ do
    putNormal ("Cleaning files in " ++ resultDir)
    removeFilesAfter resultDir ["//*"]

  -- phony "all" $ do
  --   ucfiles <- getDirectoryFiles "" ["bld/*.ucbuild"]
  --   let elf_hex_binfiles = concatMap ( uC_Triple . takeBaseName ) ucfiles
  --   need $ "tests":elf_hex_binfiles

  phony "tests" $
    need $ map takeBaseName test_yamls

  yamlCfg <- newCache (readFile' >=> loadConfig)
  getFiles <- newCache $ \f ->
    liftIO $ getDirectoryFilesIO "" f

--- Helper Functions ---
  let configNameFromSource s = splitDirectories s !! 1
      _objDir s = resultDir </> configNameFromSource s
      _sourceGenDir s = resultDir </> configNameFromSource s </> "sourceGen"
      
  let configFromSource s = do
        conf <- (-#>) (configNameFromSource s) sieveForYamlFile allConfigs
        yamlCfg conf

--- Helper Functions --

-- hex, bin and elf dependencies work in the
-- fashion, that the 'configuration name' is in the file path of
-- the 'out' file. Using this it's possible to find the cached results
-- of parsing the yaml configuration file 
-- 
-- This is an ugly design, which passes the
-- parameter through the filename. This could be fixed by ... 
-- defining a yaml config variable, that depends on what the input
-- of the YACBS.hs call is
  
  resultDir <//> "*.hex" %> \out -> do
    let elf_file = out -<.> "elf"
    need [elf_file]
    -- c is the yaml build configuration
    c <- configFromSource out :: Action Config
    cmd_ (ccObjCopy c) "-O ihex" elf_file [out]

  resultDir <//> "*.bin" %> \out -> do
    let elf_file = out -<.> "elf"
    need [elf_file]
    c <- configFromSource out :: Action Config
    cmd_ (ccObjCopy c) "-O binary" elf_file [out]

  resultDir <//> "*.elf" %> \out -> do
    c <- configFromSource out :: Action Config
    allSources <- getFiles (sourceFiles c)
    allHeaders <- getFiles (headerFiles c)
    let os = [_objDir out </> source -<.> "o" | source <- allSources]
        gcc = cc c
        map_file = out -<.> "map"
    need (linkerFile c: os)
    cmd_ gcc "-o" [out] os ("-T" ++ linkerFile c) ("-Wl,-Map," ++ map_file) (ccLinkOptions c) (ccLinkLibs c)
    cmd_ (ccSize c) "--format=berkeley -d" [out]

  resultDir <//> "*.o" %> \out -> do
    c <- configFromSource out :: Action Config
    allSources <- getDirectoryFiles "" (sourceFiles c)
    allHeaders <- getDirectoryFiles "" (headerFiles c)
    sourceFile <- if takeDirectory out == _sourceGenDir out
      then return (out -<.> "c")
      else (-#>) out (sieveSourceOfObjFile (_objDir out)) allSources
    need [sourceFile]
    let uniqueDirs = reverse $ nub (map dropFileName (allHeaders ++ allSources))
        include_dirs = map (makeInclude . fromFilePath) uniqueDirs
        ending = takeExtension sourceFile
        m = out -<.> "m"
        asmOpts = unwords (ccAsmOptions c ++ include_dirs)
        cOpts = unwords (ccCompileOptions c ++ include_dirs)
        gcc = cc c
        asm = cmd_ gcc "-MMD -MF" [m] asmOpts sourceFile "-o" [out]
        cCmd = cmd_ gcc sourceFile "-MMD -MF" [m] cOpts "-o" [out]
    case ending of
      ".c" ->
        cCmd
      ".S" ->
        asm
      ".asm" ->
        asm
      _ -> error ("Not A Valid FileEnding " ++ show ending)
    neededMakefileDependencies m

  resultDir <//> "run_*.c" %> \out -> do
    let runnerTemplate =  "bld" </> "runner_template.c"
    c <- configFromSource out :: Action Config
    allSources <- getDirectoryFiles "" (sourceFiles c)
    test_source <- (-#>) out sieveTestSourceOfRunner allSources
    generateRunner test_source runnerTemplate out

  "test_*" %> \out -> do
    let executable = resultDir </> takeBaseName out </> out -<.> exe
    need [executable]
    cmd_ executable

  -- A rule that matches ("resultDir" <//> "test_*"), but not (resultDir <//> "*.o")
  (\f -> ((resultDir <//> "test_*" <.> exe) ?== f) && not ((resultDir <//> "*.o") ?== f)) ?> \out -> do
    c <- configFromSource out :: Action Config
    sources <- getDirectoryFiles "" (sourceFiles c)
    let sourceGenDir = _sourceGenDir out
        runner_c = sourceGenDir </> testToRunner out <.> "c"
        _objectFiles = [_objDir out </> source -<.> "o" | source <- sources]
        objectFiles = (runner_c -<.> "o"):_objectFiles
        gcc = cc c
    need [runner_c]
    need objectFiles
    cmd_ gcc "-o" [out] objectFiles (ccLinkOptions c) (ccLinkLibs c)

main :: IO()
main = shakeIT
