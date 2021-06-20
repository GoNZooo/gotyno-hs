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
    String,
    Text,
    Void,
    any,
    ask,
    error,
    fromMaybe,
    id,
    maybe,
    mconcat,
    modifyIORef,
    newIORef,
    not,
    pure,
    readFileUtf8,
    readIORef,
    runRIO,
    show,
    undefined,
    writeIORef,
    ($),
    ($>),
    (*>),
    (<$>),
    (<>),
    (==),
    (>>>),
  )
import RIO.List (find)
import qualified RIO.Set as Set
import RIO.Text (pack, unpack)
import System.IO (putStrLn)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Show.Pretty (pPrint)
import Types

data AppState = AppState
  { currentModuleName :: !Text,
    currentDefinitionNameReference :: !(IORef (Maybe DefinitionName)),
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

typeDefinitionP :: Parser TypeDefinition
typeDefinitionP = do
  keyword <- choice [string "struct", string "union"]
  char ' '
  definition <- case keyword of
    "struct" -> structP
    "union" -> unionP
    other -> reportError $ "Unknown type definition keyword: " <> unpack other
  addDefinition definition
  pure definition

structP :: Parser TypeDefinition
structP = do
  name <- definitionNameP
  setCurrentDefinitionName name
  string " {\n"
  fields <- fieldsP
  char '}'
  pure TypeDefinition {name, typeData = PlainStruct PlainStructData {fields}}

constructorsP :: Parser [PlainUnionConstructor]
constructorsP = some constructorP

constructorP :: Parser PlainUnionConstructor
constructorP = do
  string "    "
  (DefinitionName name) <- definitionNameP
  maybeColon <- optional $ string ": "
  payload <- case maybeColon of
    Just _ -> Just <$> fieldTypeP
    Nothing -> pure Nothing
  newline
  pure PlainUnionConstructor {name, payload}

unionP :: Parser TypeDefinition
unionP = do
  name <- definitionNameP
  setCurrentDefinitionName name
  string " {\n"
  constructors <- constructorsP
  char '}'
  pure TypeDefinition {name, typeData = PlainUnion PlainUnionData {constructors}}

fieldsP :: Parser [StructField]
fieldsP = some fieldP

fieldP :: Parser StructField
fieldP = do
  string "    "
  name <- fieldNameP
  string ": "
  fieldType <- fieldTypeP
  newline
  pure $ StructField {name, fieldType}

fieldNameP :: Parser Text
fieldNameP = do
  initialLowerCaseCharacter <- lowerChar
  ((initialLowerCaseCharacter :) >>> pack) <$> some alphaNumChar

definitionNameP :: Parser DefinitionName
definitionNameP = do
  initialTitleCaseCharacter <- upperChar
  ((initialTitleCaseCharacter :) >>> pack >>> DefinitionName) <$> some alphaNumChar

setCurrentDefinitionName :: DefinitionName -> Parser ()
setCurrentDefinitionName name = do
  AppState {currentDefinitionNameReference} <- ask
  writeIORef currentDefinitionNameReference (Just name)

recursiveReferenceP :: Parser DefinitionName
recursiveReferenceP = do
  AppState {currentDefinitionNameReference} <- ask
  maybeCurrentDefinitionName <- readIORef currentDefinitionNameReference
  case maybeCurrentDefinitionName of
    Just currentDefinitionName@(DefinitionName n) -> do
      _ <- string n
      pure currentDefinitionName
    Nothing ->
      reportError "Recursive reference not valid when we have no current definition name"

definitionReferenceP :: Parser TypeDefinition
definitionReferenceP =
  do
    soughtName <- definitionNameP
    maybeDefinition <- getDefinition soughtName
    maybe
      (reportError $ mconcat ["Unknown type reference: ", unpack $ unDefinitionName soughtName])
      pure
      maybeDefinition

getDefinition :: DefinitionName -> Parser (Maybe TypeDefinition)
getDefinition name = do
  AppState {definitionsReference} <- ask
  definitions <- readIORef definitionsReference
  pure $ find (\TypeDefinition {name = definitionName} -> name == definitionName) definitions

addDefinition :: TypeDefinition -> Parser ()
addDefinition definition@TypeDefinition {name = DefinitionName definitionName} = do
  AppState {definitionsReference} <- ask
  definitions <- readIORef definitionsReference
  if not (hasDefinition definition definitions)
    then modifyIORef definitionsReference (definition :)
    else reportError $ "Duplicate definition with name '" <> unpack definitionName <> "'"

hasDefinition :: TypeDefinition -> [TypeDefinition] -> Bool
hasDefinition TypeDefinition {name} = any (\TypeDefinition {name = name'} -> name == name')

fieldTypeP :: Parser FieldType
fieldTypeP =
  choice
    [ LiteralType <$> literalP,
      BasicType <$> basicTypeValueP,
      RecursiveReferenceType <$> recursiveReferenceP,
      DefinitionReferenceType <$> definitionReferenceP,
      ComplexType <$> complexTypeP
    ]

reportError :: String -> Parser a
reportError = ErrorFail >>> Set.singleton >>> fancyFailure

basicTypeValueP :: Parser BasicTypeValue
basicTypeValueP = choice [uintP, intP, booleanP, basicStringP]

complexTypeP :: Parser ComplexTypeValue
complexTypeP = choice [sliceTypeP, arrayTypeP, optionalTypeP, pointerTypeP]

sliceTypeP :: Parser ComplexTypeValue
sliceTypeP = SliceType <$> precededBy (string "[]") fieldTypeP

arrayTypeP :: Parser ComplexTypeValue
arrayTypeP = do
  size <- between (char '[') (char ']') decimal
  ArrayType size <$> fieldTypeP

optionalTypeP :: Parser ComplexTypeValue
optionalTypeP = OptionalType <$> precededBy (char '?') fieldTypeP

pointerTypeP :: Parser ComplexTypeValue
pointerTypeP = PointerType <$> precededBy (char '*') fieldTypeP

precededBy :: Parser ignored -> Parser a -> Parser a
precededBy precededParser parser = do
  _ <- precededParser
  parser

uintP :: Parser BasicTypeValue
uintP = do
  char 'U'
  size <- decimal
  case size of
    8 -> pure U8
    16 -> pure U16
    32 -> pure U32
    64 -> pure U64
    128 -> pure U128
    other -> reportError $ "Invalid size for Ux: " <> show @Int other

intP :: Parser BasicTypeValue
intP = do
  char 'I'
  size <- decimal
  case size of
    8 -> pure I8
    16 -> pure I16
    32 -> pure I32
    64 -> pure I64
    128 -> pure I128
    other -> reportError $ "Invalid size for Ix: " <> show @Int other

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
  definitionsReference <- newIORef []
  currentDefinitionNameReference <- newIORef Nothing
  pure
    AppState
      { currentModuleName = "test.gotyno",
        definitionsReference,
        currentDefinitionNameReference
      }

recruiterType :: TypeDefinition
recruiterType =
  TypeDefinition
    { name = DefinitionName "Recruiter",
      typeData =
        PlainStruct
          ( PlainStructData
              { fields =
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

exampleContent :: IO Text
exampleContent = readFileUtf8 "basic.gotyno"
