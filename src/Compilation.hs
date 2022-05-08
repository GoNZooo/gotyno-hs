module Compilation where

import qualified CodeGeneration.FSharp as FSharp
import qualified CodeGeneration.Haskell as Haskell
import qualified CodeGeneration.Python as Python
import qualified CodeGeneration.TypeScript as TypeScript
import qualified Data.Text.IO as TextIO
import qualified Parsing
import RIO
import qualified RIO.Directory as Directory
import qualified RIO.FilePath as FilePath
import qualified RIO.Text as Text
import qualified RIO.Time as Time
import Types

compile :: Options -> IO CompilationState
compile
  options@Options
    { languages = Languages {typescript, fsharp, python, haskell},
      inputs
    } = do
    beforeTotal <- Time.getCurrentTime
    beforeParsing <- Time.getCurrentTime
    maybeModules <- Parsing.parseModules inputs
    afterParsing <- Time.getCurrentTime
    case maybeModules of
      Right modules -> do
        maybeTsResults <- forM typescript $ \destination -> do
          beforeOutput <- Time.getCurrentTime
          moduleStatistics <-
            outputLanguage TypeScriptOutput modules TypeScript.outputModule "ts" destination
          afterOutput <- Time.getCurrentTime
          pure (Time.diffUTCTime afterOutput beforeOutput, moduleStatistics)
        maybeFsResults <- forM fsharp $ \destination -> do
          beforeOutput <- Time.getCurrentTime
          moduleStatistics <-
            outputLanguage FSharpOutput modules FSharp.outputModule "fs" destination
          afterOutput <- Time.getCurrentTime
          pure (Time.diffUTCTime afterOutput beforeOutput, moduleStatistics)
        maybePythonResults <- forM python $ \destination -> do
          beforeOutput <- Time.getCurrentTime
          moduleStatistics <-
            outputLanguage PythonOutput modules Python.outputModule "py" destination
          afterOutput <- Time.getCurrentTime
          pure (Time.diffUTCTime afterOutput beforeOutput, moduleStatistics)
        maybeHaskellResults <- forM haskell $ \destination -> do
          beforeOutput <- Time.getCurrentTime
          moduleStatistics <-
            outputLanguage HaskellOutput modules Haskell.outputModule "hs" destination
          afterOutput <- Time.getCurrentTime
          pure (Time.diffUTCTime afterOutput beforeOutput, moduleStatistics)
        afterTotal <- Time.getCurrentTime
        let totalTime = Time.diffUTCTime afterTotal beforeTotal
            parsingTime = Time.diffUTCTime afterParsing beforeParsing
            languageData = catMaybes [maybeTsResults, maybeFsResults, maybePythonResults]
            moduleStatistics = languageData & map snd & mconcat
            languageTimes =
              LanguageOutputStatistics
                { typescriptTime = fmap fst maybeTsResults,
                  fsharpTime = fmap fst maybeFsResults,
                  pythonTime = fmap fst maybePythonResults,
                  haskellTime = fmap fst maybeHaskellResults
                }
            compilation =
              SuccessfulCompilation {totalTime, parsingTime, moduleStatistics, languageTimes}
        pure $ CompilationState {state = Right compilation, options}
      Left errors ->
        pure $ CompilationState {state = Left $ FailedCompilation errors, options}

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
      forM outputs $ \(Module {sourceFile}, output) -> do
        let pathForOutput = FilePath.replaceExtensions sourceFile extension
        beforeWrite <- Time.getCurrentTime
        writeFileUtf8 pathForOutput output
        afterWrite <- Time.getCurrentTime
        let name = sourceFile & FilePath.takeBaseName & Text.pack
            path = pathForOutput
            time = Time.diffUTCTime afterWrite beforeWrite
        pure ModuleStatistics {name, path, time, language}
    OutputPath outputDirectory -> do
      forM outputs $ \(Module {sourceFile}, output) -> do
        let pathForOutput =
              FilePath.replaceDirectory sourceFile outputDirectory
                & flip FilePath.replaceExtensions extension
            basePath = FilePath.takeDirectory pathForOutput
        beforeWrite <- Time.getCurrentTime
        Directory.createDirectoryIfMissing True basePath
        writeFileUtf8 pathForOutput output
        afterWrite <- Time.getCurrentTime
        let name = sourceFile & FilePath.takeBaseName & Text.pack
            path = pathForOutput
            time = Time.diffUTCTime afterWrite beforeWrite
        pure ModuleStatistics {name, path, time, language}
