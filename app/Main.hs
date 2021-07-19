module Main where

import qualified Library
import Options.Applicative
import Prelude

-- This `main` function just delegates to the library's definition of `main`
main :: IO ()
main = do
  let parserOptions =
        info
          (parseOptions <**> helper)
          ( fullDesc <> progDesc programDescription
              <> header ("gotyno - " <> programDescription)
          )
      programDescription = "Compile type definitions into encoders/decoders for languages"
  options <- execParser parserOptions

  Library.runMain options

parseOptions :: Parser Library.Options
parseOptions =
  Library.Options
    <$> parseLanguages
    <*> switch (long "watch" <> short 'w' <> help "Watch files and recompile automatically")
    <*> switch (long "verbose" <> short 'v' <> help "Output info about compilation")
    <*> some (argument str (metavar "GOTYNOFILE"))

parseLanguages :: Parser Library.Languages
parseLanguages =
  Library.Languages
    <$> option
      (maybeReader parseOutputDestination)
      ( long "typescript"
          <> long "ts"
          <> help "Set TypeScript output"
          <> value Nothing
          <> metavar "=|-|PATH"
      )
    <*> option
      (maybeReader parseOutputDestination)
      ( long "fsharp"
          <> long "fs"
          <> help "Set FSharp output"
          <> value Nothing
          <> metavar "=|-|PATH"
      )
    <*> option
      (maybeReader parseOutputDestination)
      ( long "python"
          <> long "py"
          <> help "Set Python output"
          <> value Nothing
          <> metavar "=|-|PATH"
      )

parseLanguageSetting :: Parser (Maybe Library.OutputDestination)
parseLanguageSetting = undefined

parseOutputDestination :: String -> Maybe (Maybe Library.OutputDestination)
parseOutputDestination "=" = pure $ Just Library.SameAsInput
parseOutputDestination "-" = pure $ Just Library.StandardOut
parseOutputDestination other = pure $ Just $ Library.OutputPath other
