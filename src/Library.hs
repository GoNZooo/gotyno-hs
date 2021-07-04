module Library where

import qualified CodeGeneration.FSharp as FSharp
import qualified CodeGeneration.TypeScript as TypeScript
import qualified Data.Text.IO as TextIO
import qualified Parsing
import RIO
import qualified RIO.Directory as Directory
import qualified RIO.FilePath as FilePath
import Types

data OutputDestination
  = SameAsInput
  | OutputPath !FilePath
  | StandardOut
  deriving (Eq, Show)

data Languages = Languages
  { typescript :: !(Maybe OutputDestination),
    fsharp :: !(Maybe OutputDestination)
  }
  deriving (Eq, Show)

data Options = Options
  { languages :: !Languages,
    watchMode :: !Bool,
    verbose :: !Bool,
    inputs :: ![FilePath]
  }
  deriving (Eq, Show)

runMain :: Options -> IO ()
runMain Options {languages = Languages {typescript, fsharp}, inputs} = do
  modules <- Parsing.parseModules inputs

  forM_ typescript $ outputLanguage modules TypeScript.outputModule "ts"
  forM_ fsharp $ outputLanguage modules FSharp.outputModule "fs"

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
