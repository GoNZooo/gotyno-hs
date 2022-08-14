module Library where

import Brick
import Brick.BChan
import qualified CodeGeneration.FSharp as FSharp
import qualified CodeGeneration.Haskell as Haskell
import qualified CodeGeneration.Kotlin as Kotlin
import qualified CodeGeneration.Python as Python
import qualified CodeGeneration.TypeScript as TypeScript
import Compilation
import qualified Graphics.Vty as Vty
import qualified Parsing
import Qtility
import qualified RIO.Directory as Directory
import qualified RIO.FilePath as FilePath
import qualified RIO.List as List
import qualified RIO.Time as Time
import qualified System.FSNotify as FSNotify
import Types
import qualified UI
import Prelude (print, putStrLn)

runMain :: Options -> IO ()
runMain options@Options {_optionsWatchMode = True} = do
  putStrLn "Starting watch mode"
  watchInputsWithTUI options
runMain
  Options
    { _optionsLanguages =
        Languages
          { _languagesTypescript = typescript,
            _languagesFsharp = fsharp,
            _languagesPython = python,
            _languagesHaskell = haskell,
            _languagesKotlin = kotlin
          },
      _optionsInputs = inputs,
      _optionsVerbose = verbose
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
        startHaskell <- Time.getCurrentTime
        forM_ haskell $
          outputLanguage HaskellOutput modules Haskell.outputModule "hs" >>> void
        endHaskell <- Time.getCurrentTime
        startKotlin <- Time.getCurrentTime
        forM_ kotlin $
          outputLanguage KotlinOutput modules Kotlin.outputModule "kt" >>> void
        endKotlin <- Time.getCurrentTime
        end <- Time.getCurrentTime
        let diff = Time.diffUTCTime end start
            diffParsing = Time.diffUTCTime postParsing start
            diffTS = Time.diffUTCTime endTS startTS
            diffFS = Time.diffUTCTime endFS startFS
            diffPython = Time.diffUTCTime endPython startPython
            diffHaskell = Time.diffUTCTime endHaskell startHaskell
            diffKotlin = Time.diffUTCTime endKotlin startKotlin
        when verbose $ do
          putStrLn $ "Parsing took: " <> show diffParsing
          putStrLn $ "Outputting TypeScript took: " <> show diffTS
          putStrLn $ "Outputting FSharp took: " <> show diffFS
          putStrLn $ "Outputting Python took: " <> show diffPython
          putStrLn $ "Outputting Haskell took: " <> show diffHaskell
          putStrLn $ "Outputting Kotlin took: " <> show diffKotlin
          putStrLn $ "Entire compilation took: " <> show diff
      Left errors -> forM_ errors putStrLn

watchInputsWithTUI :: Options -> IO ()
watchInputsWithTUI options@Options {_optionsInputs = relativeInputs, _optionsVerbose = verbose} = do
  compilationState <- compile options
  inputs <- traverse Directory.makeAbsolute relativeInputs
  let debounceInterval = 0.01 :: Time.NominalDiffTime
      fsNotifyConfig =
        FSNotify.defaultConfig {FSNotify.confDebounce = FSNotify.Debounce debounceInterval}
      inputDirectories = inputs & fmap FilePath.takeDirectory & List.nub
  compilationStateChannel <- newBChan 5
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
                      ^. compilationStateState
                      & either UI.CompilationFailed UI.CompilationSucceeded
              writeBChan compilationStateChannel compilationEvent
          )
      forever $ threadDelay 1000000

  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just compilationStateChannel) UI.app compilationState
