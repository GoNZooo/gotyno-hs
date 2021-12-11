module Library where

import Brick
import Brick.BChan
import qualified CodeGeneration.FSharp as FSharp
import qualified CodeGeneration.Python as Python
import qualified CodeGeneration.TypeScript as TypeScript
import Compilation
import qualified Graphics.Vty as Vty
import qualified Parsing
import RIO
import qualified RIO.Directory as Directory
import qualified RIO.FilePath as FilePath
import qualified RIO.List as List
import qualified RIO.Time as Time
import qualified System.FSNotify as FSNotify
import Types
import qualified UI
import Prelude (print, putStrLn)

runMain :: Options -> IO ()
runMain options@Options {watchMode = True} = do
  putStrLn "Starting watch mode"
  watchInputsWithTUI options
runMain
  Options
    { languages = Languages {typescript, fsharp, python},
      inputs,
      verbose
    } = do
    start <- Time.getCurrentTime
    maybeModules <- Parsing.parseModules inputs
    postParsing <- Time.getCurrentTime
    case maybeModules of
      Right modules -> do
        startTS <- Time.getCurrentTime
        forM_ typescript $
          outputLanguage TypeScriptOutput modules TypeScript.outputModule "ts" >>> void
        endTS <- Time.getCurrentTime
        startFS <- Time.getCurrentTime
        forM_ fsharp $ outputLanguage FSharpOutput modules FSharp.outputModule "fs" >>> void
        endFS <- Time.getCurrentTime
        startPython <- Time.getCurrentTime
        forM_ python $
          outputLanguage PythonOutput modules Python.outputModule "py" >>> void
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

watchInputsWithTUI :: Options -> IO ()
watchInputsWithTUI options@Options {inputs = relativeInputs, verbose} = do
  compilationStateChannel <- newBChan 5
  compilationState <- compile options
  inputs <- traverse Directory.makeAbsolute relativeInputs
  let debounceInterval = 0.01 :: Time.NominalDiffTime
      fsNotifyConfig =
        FSNotify.defaultConfig {FSNotify.confDebounce = FSNotify.Debounce debounceInterval}
      inputDirectories = inputs & fmap FilePath.takeDirectory & List.nub
  _notifyThread <- async $
    FSNotify.withManagerConf fsNotifyConfig $ \watchManager -> do
      let eventPredicate (FSNotify.Modified modifiedInput _modificationTime _someBool) =
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
              result <- compile options
              let compilationEvent =
                    result
                      & state
                      & either UI.CompilationFailed UI.CompilationSucceeded
              writeBChan compilationStateChannel compilationEvent
          )
      forever $ threadDelay 1000000

  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just compilationStateChannel) UI.app compilationState
