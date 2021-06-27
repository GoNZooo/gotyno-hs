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
    compare,
    const,
    error,
    forM_,
    fromMaybe,
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
    writeIORef,
    ($),
    ($>),
    (&),
    (*>),
    (.),
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
import qualified RIO.Text as Text
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

parseModules :: [FilePath] -> IO [Module]
parseModules files = do
  modulesReference <- newIORef []
  currentDefinitionsReference <- newIORef []
  currentImportsReference <- newIORef []
  currentDefinitionNameReference <- newIORef Nothing
  let state =
        AppState
          { currentDefinitionsReference,
            currentDefinitionNameReference,
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
  keyword <- choice $ List.map string ["struct", "untagged union", "union", "enum"]
  definition <- case keyword of
    "struct" ->
      char ' ' *> structP
    "union" -> do
      maybeTagType <- optional $ do
        _ <- char '('
        tagTypeP <* char ')'
      let tagType = fromMaybe (StandardTypeTag $ FieldName "type") maybeTagType
      char ' ' *> case tagType of
        StandardTypeTag fieldName ->
          unionP fieldName
        EmbeddedTypeTag fieldName ->
          embeddedUnionP fieldName
        NoTypeTag -> error "TODO: no type tag in union"
    "untagged union" ->
      char ' ' *> untaggedUnionP
    "enum" ->
      char ' ' *> enumerationP
    other ->
      reportError $ "Unknown type definition keyword: " <> unpack other
  addDefinition definition
  pure definition

untaggedUnionP :: Parser TypeDefinition
untaggedUnionP = do
  name <- readCurrentDefinitionName <* string " {\n"
  cases <- untaggedUnionCasesP
  char '}'
  pure $ TypeDefinition name $ UntaggedUnion cases

untaggedUnionCasesP :: Parser [FieldType]
untaggedUnionCasesP = do
  some untaggedUnionCaseP

untaggedUnionCaseP :: Parser FieldType
untaggedUnionCaseP =
  string "    " *> fieldTypeP [] <* newline

tagTypeP :: Parser TagType
tagTypeP = do
  maybeTagName <- optional $ string "tag = " *> fieldNameP
  many $ string ", "
  maybeEmbedded <- optional $ TypeTag <$> string "embedded"
  let tagField = fromMaybe (FieldName "type") maybeTagName
  pure $ maybe (StandardTypeTag tagField) (const $ EmbeddedTypeTag tagField) maybeEmbedded

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
  fields <- fieldsP typeVariables
  char '}'
  pure $ TypeDefinition name $ Struct $ GenericStruct typeVariables fields

plainStructP :: DefinitionName -> Parser TypeDefinition
plainStructP name = do
  fields <- fieldsP []
  char '}'
  pure $ TypeDefinition name $ Struct $ PlainStruct fields

constructorsP :: [TypeVariable] -> Parser [Constructor]
constructorsP = some . constructorP

constructorP :: [TypeVariable] -> Parser Constructor
constructorP typeVariables = do
  string "    "
  (DefinitionName name) <- definitionNameP
  maybeColon <- optional $ string ": "
  payload <- case maybeColon of
    Just _ -> Just <$> fieldTypeP typeVariables
    Nothing -> pure Nothing
  newline
  pure $ Constructor (ConstructorName name) payload

unionP :: FieldName -> Parser TypeDefinition
unionP typeTag = do
  name <- readCurrentDefinitionName <* char ' '
  maybeTypeVariables <- optional $ between (char '<') (char '>') typeVariablesP
  string "{\n"
  case maybeTypeVariables of
    Just typeVariables -> genericUnionP typeTag name $ List.map TypeVariable typeVariables
    Nothing -> plainUnionP typeTag name

embeddedUnionP :: FieldName -> Parser TypeDefinition
embeddedUnionP typeTag = do
  name <- readCurrentDefinitionName <* string " {\n"
  constructors <- embeddedUnionStructConstructorsP []
  _ <- char '}'
  pure $ TypeDefinition name (EmbeddedUnion typeTag constructors)

genericUnionP :: FieldName -> DefinitionName -> [TypeVariable] -> Parser TypeDefinition
genericUnionP typeTag name typeVariables = do
  constructors <- constructorsP typeVariables
  _ <- char '}'
  let union = Union typeTag unionType
      unionType = GenericUnion typeVariables constructors
  pure $ TypeDefinition name union

embeddedUnionStructConstructorsP :: [TypeVariable] -> Parser [EmbeddedConstructor]
embeddedUnionStructConstructorsP typeVariables =
  some $ embeddedUnionStructConstructorP typeVariables

embeddedUnionStructConstructorP :: [TypeVariable] -> Parser EmbeddedConstructor
embeddedUnionStructConstructorP typeVariables = do
  constructorName <- string "    " *> embeddedConstructorNameP <* string ": "
  definition <- structReferenceP typeVariables <* newline
  pure $ EmbeddedConstructor (ConstructorName constructorName) definition

structReferenceP :: [TypeVariable] -> Parser DefinitionReference
structReferenceP typeVariables = do
  definition <- definitionReferenceP typeVariables
  case definition of
    (DefinitionReference (TypeDefinition _name (Struct (PlainStruct _)))) ->
      pure definition
    (ImportedDefinitionReference _moduleName (TypeDefinition _name (Struct (PlainStruct _)))) ->
      pure definition
    (AppliedGenericReference _appliedTypes (TypeDefinition _name (Struct (PlainStruct _)))) ->
      pure definition
    ( AppliedImportedGenericReference
        _moduleName
        _appliedTypes
        (TypeDefinition _name (Struct (PlainStruct _)))
      ) -> pure definition
    other -> reportError $ mconcat ["Expected plain struct reference, got: ", show other]

embeddedConstructorNameP :: Parser Text
embeddedConstructorNameP = pack <$> some alphaNumChar

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

plainUnionP :: FieldName -> DefinitionName -> Parser TypeDefinition
plainUnionP typeTag name = do
  constructors <- constructorsP []
  _ <- char '}'
  pure $ TypeDefinition name $ Union typeTag (PlainUnion constructors)

typeVariablesP :: Parser [Text]
typeVariablesP = sepBy1 pascalWordP (string ", ")

pascalWordP :: Parser Text
pascalWordP = do
  initialUppercaseCharacter <- upperChar
  ((initialUppercaseCharacter :) >>> pack) <$> many alphaNumChar

fieldsP :: [TypeVariable] -> Parser [StructField]
fieldsP = some . fieldP

fieldP :: [TypeVariable] -> Parser StructField
fieldP typeVariables = do
  string "    "
  name <- fieldNameP
  string ": "
  fieldType <- fieldTypeP typeVariables
  newline
  pure $ StructField name fieldType

fieldNameP :: Parser FieldName
fieldNameP = do
  initialLowerCaseCharacter <- lowerChar
  ((initialLowerCaseCharacter :) >>> pack >>> FieldName) <$> some (alphaNumChar <|> char '_')

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

definitionReferenceP :: [TypeVariable] -> Parser DefinitionReference
definitionReferenceP typeVariables = do
  definitions <- getDefinitions
  let definitionNames =
        definitions
          & List.map (\(TypeDefinition (DefinitionName n) _typeData) -> n)
          & List.sortBy (\n1 n2 -> compare (Text.length n2) (Text.length n1))
  soughtName@(DefinitionName n) <- DefinitionName <$> choice (List.map string definitionNames)
  maybeDefinition <- getDefinition soughtName
  case maybeDefinition of
    Just definition -> do
      maybeTypeVariables <-
        optional $ between (char '<') (char '>') $ sepBy1 (fieldTypeP typeVariables) (string ", ")
      case maybeTypeVariables of
        Just appliedTypeVariables ->
          pure $ AppliedGenericReference appliedTypeVariables definition
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

fieldTypeP :: [TypeVariable] -> Parser FieldType
fieldTypeP typeVariables =
  choice
    [ LiteralType <$> literalP,
      BasicType <$> basicTypeValueP,
      ComplexType <$> complexTypeP typeVariables,
      TypeVariableReferenceType <$> typeVariableReferenceP typeVariables,
      DefinitionReferenceType <$> definitionReferenceP typeVariables,
      DefinitionReferenceType <$> importedReferenceP typeVariables,
      RecursiveReferenceType <$> recursiveReferenceP
    ]

typeVariableReferenceP :: [TypeVariable] -> Parser TypeVariable
typeVariableReferenceP typeVariables =
  TypeVariable <$> choice (List.map (\(TypeVariable t) -> string t) typeVariables)

importedReferenceP :: [TypeVariable] -> Parser DefinitionReference
importedReferenceP typeVariables = do
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
            optional $ between (char '<') (char '>') $ sepBy1 (fieldTypeP typeVariables) (string ", ")
          pure $ case maybeTypeVariables of
            Just appliedTypeVariables ->
              AppliedImportedGenericReference
                (ModuleName moduleName)
                (AppliedTypes appliedTypeVariables)
                definition
            Nothing ->
              ImportedDefinitionReference sourceModule $ TypeDefinition foundDefinitionName typeData
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
basicTypeValueP = choice [uintP, intP, floatP, booleanP, basicStringP]

complexTypeP :: [TypeVariable] -> Parser ComplexTypeValue
complexTypeP typeVariables =
  choice
    [ sliceTypeP typeVariables,
      arrayTypeP typeVariables,
      optionalTypeP typeVariables,
      pointerTypeP typeVariables
    ]

sliceTypeP :: [TypeVariable] -> Parser ComplexTypeValue
sliceTypeP typeVariables = SliceType <$> precededBy (string "[]") (fieldTypeP typeVariables)

arrayTypeP :: [TypeVariable] -> Parser ComplexTypeValue
arrayTypeP typeVariables = do
  size <- between (char '[') (char ']') decimal
  ArrayType size <$> fieldTypeP typeVariables

optionalTypeP :: [TypeVariable] -> Parser ComplexTypeValue
optionalTypeP typeVariables = OptionalType <$> precededBy (char '?') (fieldTypeP typeVariables)

pointerTypeP :: [TypeVariable] -> Parser ComplexTypeValue
pointerTypeP typeVariables = PointerType <$> precededBy (char '*') (fieldTypeP typeVariables)

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

floatP :: Parser BasicTypeValue
floatP = do
  int <- choice [string "F32", "F64"]
  case int of
    "F32" -> pure F32
    "F64" -> pure F64
    other -> reportError $ "Invalid size for Fx: " <> unpack other

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
