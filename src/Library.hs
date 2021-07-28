module Library where

import qualified CodeGeneration.FSharp as FSharp
import qualified CodeGeneration.Python as Python
import qualified CodeGeneration.TypeScript as TypeScript
import qualified Data.Text.IO as TextIO
import qualified Parsing
import RIO
import qualified RIO.Directory as Directory
import qualified RIO.FilePath as FilePath
import qualified RIO.List as List
import qualified RIO.Time as Time
import qualified System.FSNotify as FSNotify
import Types
import Prelude (print, putStrLn)

data OutputDestination
  = SameAsInput
  | OutputPath !FilePath
  | StandardOut
  deriving (Eq, Show)

data Languages
  = Languages
      { typescript :: !(Maybe OutputDestination),
        fsharp :: !(Maybe OutputDestination),
        python :: !(Maybe OutputDestination)
      }
  deriving (Eq, Show)

data Options
  = Options
      { languages :: !Languages,
        watchMode :: !Bool,
        verbose :: !Bool,
        inputs :: ![FilePath]
      }
  deriving (Eq, Show)

runMain :: Options -> IO ()
runMain
  Options
    { languages = languages@Languages {typescript, fsharp, python},
      inputs,
      watchMode,
      verbose
    } = do
    when watchMode $ do
      watchInputs inputs languages verbose
    start <- Time.getCurrentTime
    maybeModules <- Parsing.parseModules inputs
    postParsing <- Time.getCurrentTime
    case maybeModules of
      Right modules -> do
        startTS <- Time.getCurrentTime
        forM_ typescript $ outputLanguage modules TypeScript.outputModule "ts"
        endTS <- Time.getCurrentTime
        startFS <- Time.getCurrentTime
        forM_ fsharp $ outputLanguage modules FSharp.outputModule "fs"
        endFS <- Time.getCurrentTime
        startPython <- Time.getCurrentTime
        forM_ python $ outputLanguage modules Python.outputModule "py"
        endPython <- Time.getCurrentTime
        end <- Time.getCurrentTime
        let diff = Time.diffUTCTime end start
            diffParsing = Time.diffUTCTime postParsing start
            diffTS = Time.diffUTCTime endTS startTS
            diffFS = Time.diffUTCTime endFS startFS
            diffPython = Time.diffUTCTime endPython startPython
        when verbose $ do
          putStrLn $ "Parsing took: " <> show diffParsing
          putStrLn $ "Outputting TypeScript took: " <> show diffTS
          putStrLn $ "Outputting FSharp took: " <> show diffFS
          putStrLn $ "Outputting Python took: " <> show diffPython
          putStrLn $ "Entire compilation took: " <> show diff
      Left errors -> forM_ errors putStrLn

outputLanguage :: [Module] -> (Module -> Text) -> FilePath -> OutputDestination -> IO ()
outputLanguage modules outputFunction extension outputDestination = do
  let outputs = modules & fmap outputFunction & zip modules
  case outputDestination of
    StandardOut -> do
      outputs & reverse & forM_ $ \(_module, output) -> TextIO.putStrLn output
    SameAsInput -> do
      forM_ outputs $ \(Module {sourceFile}, output) -> do
        let pathForOutput = FilePath.replaceExtensions sourceFile extension
        writeFileUtf8 pathForOutput output
    OutputPath outputDirectory -> do
      forM_ outputs $ \(Module {sourceFile}, output) -> do
        let pathForOutput =
              FilePath.replaceDirectory sourceFile outputDirectory
                & flip FilePath.replaceExtensions extension
            basePath = FilePath.takeDirectory pathForOutput
        Directory.createDirectoryIfMissing True basePath
        writeFileUtf8 pathForOutput output

watchInputs :: [FilePath] -> Languages -> Bool -> IO ()
watchInputs relativeInputs Languages {typescript, fsharp, python} verbose = do
  inputs <- traverse Directory.makeAbsolute relativeInputs
  let compileEverything = do
        maybeModules <- Parsing.parseModules relativeInputs
        case maybeModules of
          Right modules -> do
            forM_ typescript $ outputLanguage modules TypeScript.outputModule "ts"
            forM_ fsharp $ outputLanguage modules FSharp.outputModule "fs"
            forM_ python $ outputLanguage modules Python.outputModule "py"
          Left errors ->
            forM_ errors putStrLn
  compileEverything
  FSNotify.withManager $ \watchManager -> do
    let inputDirectories = inputs & fmap FilePath.takeDirectory & List.nub
        eventPredicate (FSNotify.Modified modifiedInput _modificationTime _someBool) =
          modifiedInput `elem` inputs
        eventPredicate _otherEvents = False
    forM_ inputDirectories $ \inputDirectory -> do
      putStrLn $ "Watching directory: '" <> inputDirectory <> "'"
      FSNotify.watchDir
        watchManager
        inputDirectory
        eventPredicate
        ( \event -> do
            when verbose (print event)
            compileEverything
        )
    forever $ threadDelay 1000000
