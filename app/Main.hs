{-# LANGUAGE TemplateHaskell #-}

module Main where

import Library
import Options.Applicative.Simple
import qualified Paths_gotyno_hs
import Types hiding (Parser)
import Prelude

-- This `main` function just delegates to the library's definition of `main`
main :: IO ()
main = do
  let programDescription = "Compile type definitions into encoders/decoders for languages"
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_gotyno_hs.version)
      "gotyno-hs"
      programDescription
      parseOptions
      empty

  runMain options

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseLanguages
    <*> switch (long "watch" <> short 'w' <> help "Watch files and recompile automatically")
    <*> switch (long "verbose" <> short 'v' <> help "Output info about compilation")
    <*> some (argument str (metavar "GOTYNOFILE"))

parseLanguages :: Parser Languages
parseLanguages =
  Languages
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
    <*> option
      (maybeReader parseOutputDestination)
      ( long "haskell"
          <> long "hs"
          <> help "Set Haskell output"
          <> value Nothing
          <> metavar "=|-|PATH"
      )
    <*> option
      (maybeReader parseOutputDestination)
      ( long "kotlin"
          <> long "kt"
          <> help "Set Kotlin output"
          <> value Nothing
          <> metavar "=|-|PATH"
      )

parseOutputDestination :: String -> Maybe (Maybe OutputDestination)
parseOutputDestination "=" = pure $ Just SameAsInput
parseOutputDestination "-" = pure $ Just StandardOut
parseOutputDestination other = pure $ Just $ OutputPath other
