module Parsing where

import RIO
  ( Bool (..),
    Either (..),
    FilePath,
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
    forM_,
    fromMaybe,
    mconcat,
    modifyIORef,
    newIORef,
    not,
    pure,
    readFileUtf8,
    readIORef,
    runRIO,
    show,
    writeIORef,
    ($),
    ($>),
    (<$>),
    (<*),
    (<>),
    (==),
    (>>>),
  )
import qualified RIO.FilePath as FilePath
import qualified RIO.List as List
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
    currentTypeVariablesReference :: !(IORef [TypeVariable]),
    currentDefinitionsReference :: !(IORef [TypeDefinition]),
    currentDefinitionNameReference :: !(IORef (Maybe DefinitionName))
  }

type Parser = ParsecT Void Text (RIO AppState)

parseModules :: [FilePath] -> IO [Module]
parseModules files = do
  modulesReference <- newIORef []
  currentDefinitionsReference <- newIORef []
  currentTypeVariablesReference <- newIORef []
  currentImportsReference <- newIORef []
  currentDefinitionNameReference <- newIORef Nothing
  let state =
        AppState
          { currentDefinitionsReference,
            currentDefinitionNameReference,
            currentTypeVariablesReference,
            currentImportsReference,
            modulesReference
          }
  forM_ files $ \f -> do
    let (moduleName', _extension) = FilePath.splitExtension f
        moduleName = ModuleName $ pack moduleName'
    fileContents <- readFileUtf8 f
    maybeModule <- run state fileContents $moduleP moduleName
    case maybeModule of
      Right module' -> addModule module' modulesReference
      Left e -> error $ mconcat ["Error parsing module '", f, "': \n", errorBundlePretty e]

  readIORef modulesReference

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

moduleP :: ModuleName -> Parser Module
moduleP name = do
  imports <- fromMaybe [] <$> optional (sepEndBy1 importP newline <* newline)
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
  importName <- some (alphaNumChar <|> char '_')
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
  pure $ List.find (\Module {name = ModuleName name} -> name == pack importName) modules

addModule :: Module -> IORef [Module] -> IO ()
addModule module' modulesReference = do
  modifyIORef modulesReference (module' :)

typeDefinitionP :: Parser TypeDefinition
typeDefinitionP = do
  keyword <- choice $ List.map string ["struct", "union", "enum"]
  char ' '
  definition <- case keyword of
    "struct" -> structP
    "union" -> unionP
    "enum" -> enumerationP
    other -> reportError $ "Unknown type definition keyword: " <> unpack other
  addDefinition definition
  clearTypeVariables
  pure definition

readCurrentDefinitionName :: Parser DefinitionName
readCurrentDefinitionName = do
  name <- definitionNameP
  setCurrentDefinitionName name
  pure name

structP :: Parser TypeDefinition
structP = do
  name <- readCurrentDefinitionName <* char ' '
  maybeTypeVariables <- optional $ between (char '<') (char '>') typeVariablesP
  string "{\n"
  case maybeTypeVariables of
    Just typeVariables -> genericStructP name $ List.map TypeVariable typeVariables
    Nothing -> plainStructP name

genericStructP :: DefinitionName -> [TypeVariable] -> Parser TypeDefinition
genericStructP name typeVariables = do
  addTypeVariables typeVariables
  fields <- fieldsP
  char '}'
  pure $ TypeDefinition name $ Struct $ GenericStruct typeVariables fields

plainStructP :: DefinitionName -> Parser TypeDefinition
plainStructP name = do
  fields <- fieldsP
  char '}'
  pure $ TypeDefinition name $ Struct $ PlainStruct fields

constructorsP :: Parser [Constructor]
constructorsP = some constructorP

constructorP :: Parser Constructor
constructorP = do
  string "    "
  (DefinitionName name) <- definitionNameP
  maybeColon <- optional $ string ": "
  payload <- case maybeColon of
    Just _ -> Just <$> fieldTypeP
    Nothing -> pure Nothing
  newline
  pure $ Constructor (ConstructorName name) payload

unionP :: Parser TypeDefinition
unionP = do
  name <- readCurrentDefinitionName <* char ' '
  maybeTypeVariables <- optional $ between (char '<') (char '>') typeVariablesP
  string "{\n"
  case maybeTypeVariables of
    Just typeVariables -> genericUnionP name $ List.map TypeVariable typeVariables
    Nothing -> plainUnionP name

genericUnionP :: DefinitionName -> [TypeVariable] -> Parser TypeDefinition
genericUnionP name typeVariables = do
  addTypeVariables typeVariables
  constructors <- constructorsP
  _ <- char '}'
  let union = Union (StandardTypeTag $ TypeTag "type") unionType
      unionType = GenericUnion typeVariables constructors
  pure $ TypeDefinition name union

enumerationP :: Parser TypeDefinition
enumerationP = do
  name <- definitionNameP
  setCurrentDefinitionName name
  string " {\n"
  values <- enumerationValuesP
  char '}'
  pure $ TypeDefinition name $ Enumeration values

enumerationValuesP :: Parser [EnumerationValue]
enumerationValuesP = some enumerationValueP

enumerationValueP :: Parser EnumerationValue
enumerationValueP = do
  string "    "
  identifier <- (pack >>> EnumerationIdentifier) <$> someTill alphaNumChar (string " = ")
  value <- literalP <* newline
  pure $ EnumerationValue identifier value

addTypeVariables :: [TypeVariable] -> Parser ()
addTypeVariables typeVariables = do
  AppState {currentTypeVariablesReference} <- ask
  writeIORef currentTypeVariablesReference typeVariables

clearTypeVariables :: Parser ()
clearTypeVariables = do
  AppState {currentTypeVariablesReference} <- ask
  writeIORef currentTypeVariablesReference []

plainUnionP :: DefinitionName -> Parser TypeDefinition
plainUnionP name = do
  constructors <- constructorsP
  _ <- char '}'
  let union = Union (StandardTypeTag $ TypeTag "type") (PlainUnion constructors)
  pure $ TypeDefinition name union

typeVariablesP :: Parser [Text]
typeVariablesP = sepBy1 pascalWordP (string ", ")

getTypeVariables :: Parser [TypeVariable]
getTypeVariables = do
  AppState {currentTypeVariablesReference} <- ask
  readIORef currentTypeVariablesReference

pascalWordP :: Parser Text
pascalWordP = do
  initialUppercaseCharacter <- upperChar
  ((initialUppercaseCharacter :) >>> pack) <$> many alphaNumChar

fieldsP :: Parser [StructField]
fieldsP = some fieldP

fieldP :: Parser StructField
fieldP = do
  string "    "
  name <- fieldNameP
  string ": "
  fieldType <- fieldTypeP
  newline
  pure $ StructField name fieldType

fieldNameP :: Parser FieldName
fieldNameP = do
  initialLowerCaseCharacter <- lowerChar
  ((initialLowerCaseCharacter :) >>> pack >>> FieldName) <$> some alphaNumChar

definitionNameP :: Parser DefinitionName
definitionNameP = do
  initialTitleCaseCharacter <- upperChar
  ((initialTitleCaseCharacter :) >>> pack >>> DefinitionName) <$> many alphaNumChar

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

definitionReferenceP :: Parser DefinitionReference
definitionReferenceP = do
  definitionNames <-
    List.map (\(TypeDefinition (DefinitionName n) _typeData) -> n) <$> getDefinitions
  soughtName@(DefinitionName n) <- DefinitionName <$> choice (List.map string definitionNames)
  maybeDefinition <- getDefinition soughtName
  case maybeDefinition of
    Just definition -> do
      maybeTypeVariables <-
        optional $ between (char '<') (char '>') $ sepBy1 fieldTypeP (string ", ")
      case maybeTypeVariables of
        Just typeVariables ->
          pure $ AppliedGenericReference typeVariables definition
        Nothing ->
          pure $ DefinitionReference definition
    Nothing -> reportError $ mconcat ["Unknown type reference: ", unpack n]

getDefinitions :: Parser [TypeDefinition]
getDefinitions = do
  AppState {currentDefinitionsReference} <- ask
  readIORef currentDefinitionsReference

getDefinition :: DefinitionName -> Parser (Maybe TypeDefinition)
getDefinition name = do
  AppState {currentDefinitionsReference} <- ask
  definitions <- readIORef currentDefinitionsReference
  pure $
    List.find (\(TypeDefinition definitionName _typeData) -> name == definitionName) definitions

addDefinition :: TypeDefinition -> Parser ()
addDefinition definition@(TypeDefinition (DefinitionName definitionName) _typeData) = do
  AppState {currentDefinitionsReference} <- ask
  definitions <- readIORef currentDefinitionsReference
  if not (hasDefinition definition definitions)
    then modifyIORef currentDefinitionsReference (definition :)
    else reportError $ "Duplicate definition with name '" <> unpack definitionName <> "'"

hasDefinition :: TypeDefinition -> [TypeDefinition] -> Bool
hasDefinition (TypeDefinition name _typeData) =
  any (\(TypeDefinition name' _typeData) -> name == name')

fieldTypeP :: Parser FieldType
fieldTypeP =
  choice
    [ LiteralType <$> literalP,
      BasicType <$> basicTypeValueP,
      ComplexType <$> complexTypeP,
      RecursiveReferenceType <$> recursiveReferenceP,
      TypeVariableReferenceType <$> typeVariableReferenceP,
      DefinitionReferenceType <$> definitionReferenceP,
      DefinitionReferenceType <$> importedReferenceP
    ]

typeVariableReferenceP :: Parser TypeVariable
typeVariableReferenceP = do
  typeVariables <- getTypeVariables
  TypeVariable <$> choice (List.map (\(TypeVariable t) -> string t) typeVariables)

checkForTypeVariable :: TypeVariable -> Parser Bool
checkForTypeVariable name = do
  AppState {currentTypeVariablesReference} <- ask
  (name `List.elem`) <$> readIORef currentTypeVariablesReference

importedReferenceP :: Parser DefinitionReference
importedReferenceP = do
  imports <- getImports
  moduleName <-
    choice (List.map (\(Import Module {name = ModuleName name}) -> string name) imports) <* char '.'
  definitionName@(DefinitionName n) <- definitionNameP
  maybeModule <- getImport moduleName
  case maybeModule of
    Just (Import Module {name = sourceModule, definitions}) -> do
      case List.find (\(TypeDefinition name _typeData) -> name == definitionName) definitions of
        Just definition@(TypeDefinition foundDefinitionName typeData) -> do
          maybeTypeVariables <-
            optional $ between (char '<') (char '>') $ sepBy1 fieldTypeP (string ", ")
          pure $ case maybeTypeVariables of
            Just typeVariables ->
              AppliedImportedGenericReference (ModuleName moduleName) (AppliedTypes typeVariables) definition
            Nothing ->
              ImportedDefinitionReference sourceModule foundDefinitionName typeData
        Nothing ->
          reportError $
            mconcat
              [ "Unknown definition in module '",
                unpack moduleName,
                "': ",
                unpack n
              ]
    Nothing ->
      reportError $ "Unknown module referenced, not in imports: " <> unpack moduleName

getImports :: Parser [Import]
getImports = do
  AppState {currentImportsReference} <- ask
  readIORef currentImportsReference

getImport :: Text -> Parser (Maybe Import)
getImport soughtName = do
  AppState {currentImportsReference} <- ask
  imports <- readIORef currentImportsReference
  pure $ List.find (\(Import Module {name = ModuleName name}) -> soughtName == name) imports

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

integerSizes :: [Int]
integerSizes = [8, 16, 32, 64, 128]

integerTypeParsers :: Text -> [Parser Text]
integerTypeParsers prefix = List.map (show >>> pack >>> (prefix <>) >>> string) integerSizes

uintP :: Parser BasicTypeValue
uintP = do
  uint <- choice $ integerTypeParsers "U"
  case uint of
    "U8" -> pure U8
    "U16" -> pure U16
    "U32" -> pure U32
    "U64" -> pure U64
    "U128" -> pure U128
    other -> reportError $ "Invalid size for Ux: " <> unpack other

intP :: Parser BasicTypeValue
intP = do
  int <- choice $ integerTypeParsers "I"
  case int of
    "I8" -> pure I8
    "I16" -> pure I16
    "I32" -> pure I32
    "I64" -> pure I64
    "I128" -> pure I128
    other -> reportError $ "Invalid size for Ix: " <> unpack other

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
