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
    (<*),
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
  { modulesReference :: !(IORef [Module]),
    currentImportsReference :: !(IORef [Import]),
    currentDefinitionsReference :: !(IORef [TypeDefinition]),
    currentDefinitionNameReference :: !(IORef (Maybe DefinitionName))
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

moduleP :: Text -> Parser Module
moduleP name = do
  imports <- fromMaybe [] <$> optional (some importP <* newline)
  addImports imports
  definitions <- sepBy1 typeDefinitionP (newline <* newline) <* eof
  pure Module {name, imports, definitions}

addImports :: [Import] -> Parser ()
addImports imports = do
  AppState {currentImportsReference} <- ask
  writeIORef currentImportsReference imports

importP :: Parser Import
importP = do
  string "import "
  importName <- manyTill (alphaNumChar <|> char '_') newline
  maybeModule <- getModule importName
  case maybeModule of
    Just module' -> do
      pure $ Import module'
    Nothing ->
      reportError $ "Unknown module referenced: " <> importName

getModule :: String -> Parser (Maybe Module)
getModule importName = do
  AppState {modulesReference} <- ask
  modules <- readIORef modulesReference
  pure $ find (\Module {name} -> name == pack importName) modules

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
  AppState {currentDefinitionsReference} <- ask
  definitions <- readIORef currentDefinitionsReference
  pure $ find (\TypeDefinition {name = definitionName} -> name == definitionName) definitions

addDefinition :: TypeDefinition -> Parser ()
addDefinition definition@TypeDefinition {name = DefinitionName definitionName} = do
  AppState {currentDefinitionsReference} <- ask
  definitions <- readIORef currentDefinitionsReference
  if not (hasDefinition definition definitions)
    then modifyIORef currentDefinitionsReference (definition :)
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
      ComplexType <$> complexTypeP,
      ImportedReferenceType <$> importedReferenceP
    ]

importedReferenceP :: Parser ImportedTypeDefinition
importedReferenceP = do
  moduleName <- someTill lowerChar (char '.')
  definitionName@(DefinitionName n) <- definitionNameP
  maybeModule <- getImport $ pack moduleName
  case maybeModule of
    Just (Import Module {name = sourceModule, definitions}) -> do
      case find (\TypeDefinition {name} -> name == definitionName) definitions of
        Just TypeDefinition {name = foundDefinitionName, typeData} ->
          pure $ ImportedTypeDefinition {name = foundDefinitionName, sourceModule, typeData}
        Nothing ->
          reportError $ mconcat ["Unknown definition in module '", moduleName, "': ", unpack n]
    Nothing ->
      reportError $ "Unknown module referenced, not in imports: " <> moduleName

getImport :: Text -> Parser (Maybe Import)
getImport soughtName = do
  AppState {currentImportsReference} <- ask
  imports <- readIORef currentImportsReference
  pure $ find (\(Import Module {name}) -> soughtName == name) imports

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
  modulesReference <- newIORef [basicModule]
  currentDefinitionsReference <- newIORef []
  currentImportsReference <- newIORef []
  currentDefinitionNameReference <- newIORef Nothing
  pure
    AppState
      { currentDefinitionsReference,
        currentDefinitionNameReference,
        currentImportsReference,
        modulesReference
      }

exampleContent :: IO Text
exampleContent = readFileUtf8 "importExample.gotyno"

basicModule :: Module
basicModule =
  Module
    { name = "basic",
      imports = [],
      definitions =
        [ TypeDefinition
            { name = DefinitionName {unDefinitionName = "Recruiter"},
              typeData =
                PlainStruct
                  PlainStructData
                    { fields =
                        [ StructField
                            { name = "type",
                              fieldType = LiteralType (LiteralString "Recruiter")
                            },
                          StructField {name = "name", fieldType = BasicType BasicString},
                          StructField
                            { name = "emails",
                              fieldType =
                                ComplexType
                                  (ArrayType 3 (ComplexType (OptionalType (BasicType BasicString))))
                            },
                          StructField
                            { name = "recruiter",
                              fieldType =
                                ComplexType
                                  ( OptionalType
                                      ( ComplexType
                                          ( PointerType
                                              ( RecursiveReferenceType
                                                  DefinitionName {unDefinitionName = "Recruiter"}
                                              )
                                          )
                                      )
                                  )
                            }
                        ]
                    }
            },
          TypeDefinition
            { name = DefinitionName {unDefinitionName = "GetSearchesFilter"},
              typeData =
                PlainUnion
                  PlainUnionData
                    { constructors =
                        [ PlainUnionConstructor
                            { name = "SearchesByQueryLike",
                              payload = Just (BasicType BasicString)
                            },
                          PlainUnionConstructor
                            { name = "SearchesByResultLike",
                              payload = Just (BasicType BasicString)
                            },
                          PlainUnionConstructor
                            { name = "NoSearchesFilter",
                              payload = Nothing
                            }
                        ]
                    }
            },
          TypeDefinition
            { name = DefinitionName {unDefinitionName = "SearchesParameters"},
              typeData =
                PlainStruct
                  PlainStructData
                    { fields =
                        [ StructField
                            { name = "filters",
                              fieldType =
                                ComplexType
                                  ( SliceType
                                      ( DefinitionReferenceType
                                          TypeDefinition
                                            { name =
                                                DefinitionName
                                                  { unDefinitionName = "GetSearchesFilter"
                                                  },
                                              typeData =
                                                PlainUnion
                                                  PlainUnionData
                                                    { constructors =
                                                        [ PlainUnionConstructor
                                                            { name = "SearchesByQueryLike",
                                                              payload = Just (BasicType BasicString)
                                                            },
                                                          PlainUnionConstructor
                                                            { name = "SearchesByResultLike",
                                                              payload = Just (BasicType BasicString)
                                                            },
                                                          PlainUnionConstructor
                                                            { name = "NoSearchesFilter",
                                                              payload = Nothing
                                                            }
                                                        ]
                                                    }
                                            }
                                      )
                                  )
                            }
                        ]
                    }
            }
        ]
    }
