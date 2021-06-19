{-# LANGUAGE TypeApplications #-}

module Parsing where

import RIO
  ( Bool (..),
    Either (..),
    IO,
    IORef,
    Int,
    Maybe (..),
    RIO,
    Show,
    Text,
    Void,
    ask,
    error,
    fromMaybe,
    maybe,
    mconcat,
    modifyIORef,
    newIORef,
    pure,
    readIORef,
    runRIO,
    show,
    undefined,
    writeIORef,
    ($),
    ($>),
    (<$>),
    (<>),
    (==),
    (>>>),
  )
import RIO.List (find)
import RIO.Text (pack, unpack)
import System.IO (putStrLn)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Show.Pretty (pPrint)
import Types

data AppState = AppState
  { currentModuleName :: !Text,
    definitionsReference :: !(IORef [TypeDefinition])
  }

type Parser = ParsecT Void Text (RIO AppState)

run :: AppState -> Text -> Parser a -> IO (Either (ParseErrorBundle Text Void) a)
run state text parser = do
  let parserResult = runParserT parser "" text
  runRIO state parserResult

test :: (Show a) => AppState -> Text -> Parser a -> IO ()
test state text parser = do
  result <- run state text parser
  case result of
    Right successValue -> pPrint successValue
    Left e -> putStrLn $ errorBundlePretty e

fieldTypeP :: Parser FieldType
fieldTypeP =
  choice
    [ LiteralType <$> literalP,
      BasicType <$> basicTypeValueP,
      DefinitionReferenceType <$> definitionReferenceP
    ]

getDefinition :: DefinitionName -> Parser (Maybe TypeDefinition)
getDefinition name = do
  AppState {definitionsReference} <- ask
  definitions <- readIORef definitionsReference
  pure $ find (\TypeDefinition {name = definitionName} -> name == definitionName) definitions

definitionReferenceP :: Parser TypeDefinition
definitionReferenceP = do
  initialTitleCaseCharacter <- upperChar
  soughtName <-
    ((initialTitleCaseCharacter :) >>> pack >>> DefinitionName) <$> someTill alphaNumChar newline
  fromMaybe
    ( error $
        mconcat ["Unknown type reference: ", unpack $ unDefinitionName soughtName]
    )
    <$> getDefinition soughtName

basicTypeValueP :: Parser BasicTypeValue
basicTypeValueP = choice [uintP, intP, booleanP, basicStringP]

uintP :: Parser BasicTypeValue
uintP = do
  char 'U'
  size <- decimal
  pure $ case size of
    8 -> U8
    16 -> U16
    32 -> U32
    64 -> U64
    128 -> U128
    other -> error $ "Invalid size for Ux: " <> show @Int other

intP :: Parser BasicTypeValue
intP = do
  char 'I'
  size <- decimal
  pure $ case size of
    8 -> I8
    16 -> I16
    32 -> I32
    64 -> I64
    128 -> I128
    other -> error $ "Invalid size for Ix: " <> show @Int other

booleanP :: Parser BasicTypeValue
booleanP = string "Boolean" $> Boolean

basicStringP :: Parser BasicTypeValue
basicStringP = string "String" $> BasicString

literalP :: Parser LiteralTypeValue
literalP = choice [literalStringP, literalIntegerP, literalFloatP, literalBooleanP]

literalStringP :: Parser LiteralTypeValue
literalStringP = (pack >>> LiteralString) <$> between (char '"') (char '"') (many alphaNumChar)

literalIntegerP :: Parser LiteralTypeValue
literalIntegerP = LiteralInteger <$> decimal

literalFloatP :: Parser LiteralTypeValue
literalFloatP = LiteralFloat <$> float

literalBooleanP :: Parser LiteralTypeValue
literalBooleanP = LiteralBoolean <$> choice [trueP, falseP]

trueP :: Parser Bool
trueP = string "true" $> True

falseP :: Parser Bool
falseP = string "false" $> False

testAppState :: IO AppState
testAppState = do
  definitionsReference <- newIORef [recruiterType]
  pure AppState {currentModuleName = "test.gotyno", definitionsReference}

recruiterType :: TypeDefinition
recruiterType =
  TypeDefinition
    { name = DefinitionName "Recruiter",
      typeData =
        PlainStruct
          ( PlainStructData
              { name = "Recruiter",
                fields =
                  [ StructField
                      { name = "type",
                        fieldType = LiteralType (LiteralString "Recruiter")
                      },
                    StructField
                      { name = "name",
                        fieldType = BasicType BasicString
                      }
                  ]
              }
          )
    }
