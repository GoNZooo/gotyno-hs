{-# LANGUAGE BangPatterns #-}

module Compilation where

import qualified CodeGeneration.FSharp as FSharp
import qualified CodeGeneration.Haskell as Haskell
import qualified CodeGeneration.Kotlin as Kotlin
import qualified CodeGeneration.Python as Python
import qualified CodeGeneration.TypeScript as TypeScript
import Control.Lens (_1, _2)
import qualified Data.Text.IO as TextIO
import qualified Parsing
import Qtility
import qualified RIO.Directory as Directory
import qualified RIO.FilePath as FilePath
import qualified RIO.Text as Text
import qualified RIO.Time as Time
import Types

compile :: Options -> IO CompilationState
compile options = do
  !beforeTotal <- Time.getCurrentTime
  !beforeParsing <- Time.getCurrentTime
  !maybeModules <- Parsing.parseModules $ options ^. optionsInputs
  !afterParsing <- Time.getCurrentTime
  case maybeModules of
    Right modules -> do
      maybeTsResults <- forM (options ^. optionsLanguages . languagesTypescript) $ \destination -> do
        !beforeOutput <- Time.getCurrentTime
        moduleStatistics <-
          outputLanguage TypeScriptOutput modules TypeScript.outputModule "ts" destination
        !afterOutput <- Time.getCurrentTime
        pure (Time.diffUTCTime afterOutput beforeOutput, moduleStatistics)
      maybeFsResults <- forM (options ^. optionsLanguages . languagesFsharp) $ \destination -> do
        !beforeOutput <- Time.getCurrentTime
        !moduleStatistics <-
          outputLanguage FSharpOutput modules FSharp.outputModule "fs" destination
        !afterOutput <- Time.getCurrentTime
        pure (Time.diffUTCTime afterOutput beforeOutput, moduleStatistics)
      maybePythonResults <- forM (options ^. optionsLanguages . languagesPython) $ \destination -> do
        !beforeOutput <- Time.getCurrentTime
        !moduleStatistics <-
          outputLanguage PythonOutput modules Python.outputModule "py" destination
        !afterOutput <- Time.getCurrentTime
        pure (Time.diffUTCTime afterOutput beforeOutput, moduleStatistics)
      maybeHaskellResults <- forM (options ^. optionsLanguages . languagesHaskell) $ \destination -> do
        !beforeOutput <- Time.getCurrentTime
        !moduleStatistics <-
          outputLanguage HaskellOutput modules Haskell.outputModule "hs" destination
        !afterOutput <- Time.getCurrentTime
        pure (Time.diffUTCTime afterOutput beforeOutput, moduleStatistics)
      maybeKotlinResults <- forM (options ^. optionsLanguages . languagesKotlin) $ \destination -> do
        !beforeOutput <- Time.getCurrentTime
        !moduleStatistics <-
          outputLanguage KotlinOutput modules Kotlin.outputModule "kt" destination
        !afterOutput <- Time.getCurrentTime
        pure (Time.diffUTCTime afterOutput beforeOutput, moduleStatistics)
      !afterTotal <- Time.getCurrentTime
      let !totalTime = Time.diffUTCTime afterTotal beforeTotal
          !parsingTime = Time.diffUTCTime afterParsing beforeParsing
          !languageData = catMaybes [maybeTsResults, maybeFsResults, maybePythonResults]
          !moduleStatistics = languageData & map snd & mconcat
          languageTimes =
            LanguageOutputStatistics
              { _languageOutputStatisticsTypescriptTime = fmap fst maybeTsResults,
                _languageOutputStatisticsFsharpTime = fmap fst maybeFsResults,
                _languageOutputStatisticsPythonTime = fmap fst maybePythonResults,
                _languageOutputStatisticsHaskellTime = fmap fst maybeHaskellResults,
                _languageOutputStatisticsKotlinTime = fmap fst maybeKotlinResults
              }
          compilation =
            SuccessfulCompilation
              { _successfulCompilationTotalTime = totalTime,
                _successfulCompilationParsingTime = parsingTime,
                _successfulCompilationModuleStatistics = moduleStatistics,
                _successfulCompilationLanguageTimes = languageTimes
              }
      pure $
        CompilationState
          { _compilationStateState = Right compilation,
            _compilationStateOptions =
              options
          }
    Left errors ->
      pure $
        CompilationState
          { _compilationStateState = Left $ FailedCompilation errors,
            _compilationStateOptions = options
          }

outputLanguage ::
  OutputLanguage ->
  [Module] ->
  (Module -> Text) ->
  FilePath ->
  OutputDestination ->
  IO [ModuleStatistics]
outputLanguage language modules outputFunction extension outputDestination = do
  let outputs = modules & fmap outputFunction & zip modules
  case outputDestination of
    StandardOut -> do
      outputs & reverse & forM_ $ \(_module, output) -> TextIO.putStrLn output
      pure []
    SameAsInput -> do
      forM outputs $ \output -> do
        let pathForOutput = FilePath.replaceExtensions sourceFile extension
            sourceFile = output ^. _1 . moduleSourceFile
        beforeWrite <- Time.getCurrentTime
        writeFileUtf8 pathForOutput $ output ^. _2
        afterWrite <- Time.getCurrentTime
        let name = sourceFile & FilePath.takeBaseName & Text.pack
            path = pathForOutput
            time = Time.diffUTCTime afterWrite beforeWrite
        pure
          ModuleStatistics
            { _moduleStatisticsName = name,
              _moduleStatisticsPath = path,
              _moduleStatisticsTime = time,
              _moduleStatisticsLanguage = language
            }
    OutputPath outputDirectory -> do
      forM outputs $ \output -> do
        let pathForOutput =
              FilePath.replaceDirectory sourceFile outputDirectory
                & flip FilePath.replaceExtensions extension
            basePath = FilePath.takeDirectory pathForOutput
            sourceFile = output ^. _1 . moduleSourceFile
        beforeWrite <- Time.getCurrentTime
        Directory.createDirectoryIfMissing True basePath
        writeFileUtf8 pathForOutput $ output ^. _2
        afterWrite <- Time.getCurrentTime
        let name = sourceFile & FilePath.takeBaseName & Text.pack
            path = pathForOutput
            time = Time.diffUTCTime afterWrite beforeWrite
        pure
          ModuleStatistics
            { _moduleStatisticsName = name,
              _moduleStatisticsPath = path,
              _moduleStatisticsTime = time,
              _moduleStatisticsLanguage = language
            }
