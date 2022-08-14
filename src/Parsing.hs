module Parsing (parseModules, test) where

import qualified CodeGeneration.Utilities as Utilities
import Qtility
import qualified RIO.FilePath as FilePath
import qualified RIO.List as List
import qualified RIO.Set as Set
import RIO.Text (pack, unpack)
import qualified RIO.Text as Text
import System.IO (putStrLn)
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (lexeme, symbol)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Show.Pretty (pPrint)
import Types

data AppState = AppState
  { modulesReference :: !(IORef [Module]),
    currentDeclarationNamesReference :: !(IORef (Set ModuleName)),
    currentDefinitionsReference :: !(IORef [TypeDefinition]),
    currentDefinitionNameReference :: !(IORef (Maybe DefinitionName))
  }

type Parser = ParsecT Void Text (RIO AppState)

parseModules :: [FilePath] -> IO (Either [String] [Module])
parseModules files = do
  modulesReference <- newIORef []
  currentDefinitionsReference <- newIORef []
  currentDeclarationNamesReference <- newIORef Set.empty
  currentDefinitionNameReference <- newIORef Nothing
  let state =
        AppState
          { currentDefinitionsReference,
            currentDefinitionNameReference,
            currentDeclarationNamesReference,
            modulesReference
          }
  results <- for files $ \f -> do
    let moduleName' = f & FilePath.takeBaseName & pack & ModuleName
    fileContents <- readFileUtf8 f
    maybeModule <- run state (moduleP moduleName' f) fileContents
    writeIORef currentDefinitionsReference mempty
    writeIORef currentDeclarationNamesReference mempty
    case maybeModule of
      Right module' -> do
        addModule module' modulesReference
        pure $ Right module'
      Left e -> pure $ Left $ mconcat ["Error parsing module '", f, "': \n", errorBundlePretty e]

  pure $ case List.partition isLeft results of
    ([], maybeModules) ->
      Right $ List.map partialFromRight maybeModules
    (errors, _modules) ->
      Left $ List.map partialFromLeft errors

run :: AppState -> Parser a -> Text -> IO (Either (ParseErrorBundle Text Void) a)
run state parser = runParserT parser "" >>> runRIO state

test :: (Show a) => AppState -> Text -> Parser a -> IO ()
test state text parser = do
  result <- run state parser text
  case result of
    Right successValue -> pPrint successValue
    Left e -> putStrLn $ errorBundlePretty e

moduleP :: ModuleName -> FilePath -> Parser Module
moduleP name sourceFile = do
  imports <- fromMaybe [] <$> optional (sepEndBy1 importP eol <* eol)
  definitions <- sepEndBy1 (typeDefinitionP imports) (some eol) <* eof
  declarationNames <- Set.toList <$> getDeclarationNames
  pure
    Module
      { _moduleName = name,
        _moduleImports = imports,
        _moduleDefinitions = definitions,
        _moduleSourceFile = sourceFile,
        _moduleDeclarationNames = declarationNames
      }

importP :: Parser Import
importP = do
  symbol "import "
  importName <- some (alphaNumChar <|> char '_')
  maybeModule <- getModule importName
  case maybeModule of
    Just module' -> do
      pure $ Import module'
    Nothing ->
      reportError $ "Unknown module referenced: " <> importName

typeDefinitionP :: [Import] -> Parser TypeDefinition
typeDefinitionP imports = do
  keyword <- choice $ symbol <$> ["struct", "untagged union", "union", "enum", "declare"]
  definition <- case keyword of
    "struct" ->
      structP imports
    "union" -> do
      maybeTagType <- optional $ do
        _ <- symbol "("
        tagTypeP <* char ')' <* some (char ' ')
      let tagType = fromMaybe (StandardTypeTag $ FieldName "type") maybeTagType
      case tagType of
        StandardTypeTag fieldName ->
          unionP imports fieldName
        EmbeddedTypeTag fieldName ->
          embeddedUnionP imports fieldName
    "untagged union" ->
      untaggedUnionP imports
    "enum" ->
      enumerationP
    "declare" ->
      declarationP
    other ->
      reportError $ "Unknown type definition keyword: " <> unpack other
  addDefinition definition
  pure definition

declarationP :: Parser TypeDefinition
declarationP = do
  externalModule <- (pack >>> ModuleName) <$> some (alphaNumChar <|> char '_') <* char '.'
  name <- readCurrentDefinitionName
  typeVariables <-
    (fromMaybe [] >>> List.map TypeVariable) <$> optional (angleBracketed typeVariablesP)
  addDeclarationName externalModule
  pure $ TypeDefinition name $ DeclaredType externalModule typeVariables

untaggedUnionP :: [Import] -> Parser TypeDefinition
untaggedUnionP imports = do
  name <- lexeme readCurrentDefinitionName <* string "{" <* eol
  cases <- untaggedUnionCasesP imports
  char '}'
  pure $ TypeDefinition name $ UntaggedUnion cases

untaggedUnionCasesP :: [Import] -> Parser [FieldType]
untaggedUnionCasesP = do
  untaggedUnionCaseP >>> some

untaggedUnionCaseP :: [Import] -> Parser FieldType
untaggedUnionCaseP imports =
  some (char ' ') *> fieldTypeP imports [] <* eol

tagTypeP :: Parser TagType
tagTypeP = do
  maybeTagName <- optional $ string "tag = " *> fieldNameP
  many $ string ", "
  maybeEmbedded <- optional $ TypeTag <$> string "embedded"
  let tagField = fromMaybe (FieldName "type") maybeTagName
  pure $ maybe (StandardTypeTag tagField) (const $ EmbeddedTypeTag tagField) maybeEmbedded

structP :: [Import] -> Parser TypeDefinition
structP imports = do
  (name, maybeTypeVariables) <- nameAndMaybeTypeVariablesP
  _ <- string "{" <* eol
  case maybeTypeVariables of
    Just typeVariables -> genericStructP imports name $ List.map TypeVariable typeVariables
    Nothing -> plainStructP imports name

genericStructP :: [Import] -> DefinitionName -> [TypeVariable] -> Parser TypeDefinition
genericStructP imports name typeVariables = do
  fields <- fieldsP imports typeVariables
  char '}'
  pure $ TypeDefinition name $ Struct $ GenericStruct typeVariables fields

plainStructP :: [Import] -> DefinitionName -> Parser TypeDefinition
plainStructP imports name = do
  fields <- fieldsP imports []
  char '}'
  pure $ TypeDefinition name $ Struct $ PlainStruct fields

constructorsP :: [Import] -> [TypeVariable] -> Parser [Constructor]
constructorsP imports typeVariables = some $ some (char ' ') *> constructorP imports typeVariables

constructorP :: [Import] -> [TypeVariable] -> Parser Constructor
constructorP imports typeVariables = do
  name <- constructorNameP
  maybeColon <- optional $ symbol ": "
  payload <- case maybeColon of
    Just _ -> Just <$> fieldTypeP imports typeVariables
    Nothing -> pure Nothing
  many (char ' ') *> eol
  pure $ Constructor (ConstructorName name) payload

constructorNameP :: Parser Text
constructorNameP = do
  firstLetter <- alphaNumChar
  rest <- many alphaNumChar
  pure $ pack $ firstLetter : rest

unionP :: [Import] -> FieldName -> Parser TypeDefinition
unionP imports typeTag = do
  (name, maybeTypeVariables) <- nameAndMaybeTypeVariablesP
  _ <- string "{" <* eol
  case maybeTypeVariables of
    Just typeVariables -> genericUnionP imports typeTag name $ List.map TypeVariable typeVariables
    Nothing -> plainUnionP imports typeTag name

embeddedUnionP :: [Import] -> FieldName -> Parser TypeDefinition
embeddedUnionP imports typeTag = do
  name <- lexeme readCurrentDefinitionName <* string "{" <* eol
  constructors <- embeddedUnionStructConstructorsP imports []
  _ <- char '}'
  pure $ TypeDefinition name (EmbeddedUnion typeTag constructors)

genericUnionP :: [Import] -> FieldName -> DefinitionName -> [TypeVariable] -> Parser TypeDefinition
genericUnionP imports typeTag name typeVariables = do
  constructors <- constructorsP imports typeVariables
  _ <- char '}'
  let union = Union typeTag unionType
      unionType = GenericUnion typeVariables constructors
  pure $ TypeDefinition name union

embeddedUnionStructConstructorsP :: [Import] -> [TypeVariable] -> Parser [EmbeddedConstructor]
embeddedUnionStructConstructorsP imports =
  embeddedUnionStructConstructorP imports >>> some

embeddedUnionStructConstructorP :: [Import] -> [TypeVariable] -> Parser EmbeddedConstructor
embeddedUnionStructConstructorP imports typeVariables = do
  constructorName' <- some (char ' ') *> embeddedConstructorNameP
  maybeDefinition <-
    choice
      [ Nothing <$ many (char ' ') <* eol,
        Just <$> (symbol ": " *> structReferenceP imports typeVariables <* many (char ' ') <* eol)
      ]
  pure $ EmbeddedConstructor (ConstructorName constructorName') maybeDefinition

structReferenceP :: [Import] -> [TypeVariable] -> Parser DefinitionReference
structReferenceP imports typeVariables = do
  definition <- definitionReferenceP imports typeVariables
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
  name <- lexeme readCurrentDefinitionName <* "{" <* eol
  values' <- enumerationValuesP
  char '}'
  pure $ TypeDefinition name $ Enumeration values'

enumerationValuesP :: Parser [EnumerationValue]
enumerationValuesP = some enumerationValueP

enumerationValueP :: Parser EnumerationValue
enumerationValueP = do
  some (char ' ')
  identifier <- (pack >>> EnumerationIdentifier) <$> someTill alphaNumChar (symbol " = ")
  value <- literalP <* many (char ' ') <* eol
  pure $ EnumerationValue identifier value

plainUnionP :: [Import] -> FieldName -> DefinitionName -> Parser TypeDefinition
plainUnionP imports typeTag name = do
  constructors <- constructorsP imports []
  _ <- char '}'
  pure $ TypeDefinition name $ Union typeTag (PlainUnion constructors)

typeVariablesP :: Parser [Text]
typeVariablesP = sepBy1 pascalWordP (string ", ")

pascalWordP :: Parser Text
pascalWordP = do
  initialUppercaseCharacter <- upperChar
  ((initialUppercaseCharacter :) >>> pack) <$> many alphaNumChar

fieldsP :: [Import] -> [TypeVariable] -> Parser [StructField]
fieldsP imports = fieldP imports >>> some

fieldP :: [Import] -> [TypeVariable] -> Parser StructField
fieldP imports typeVariables = do
  _ <- some $ char ' '
  name <- fieldNameP
  symbol ": "
  fieldType <- fieldTypeP imports typeVariables
  eol
  pure $ StructField name fieldType

fieldNameP :: Parser FieldName
fieldNameP = do
  initialAlphaChar <- lowerChar <|> upperChar
  ((initialAlphaChar :) >>> pack >>> FieldName) <$> many (alphaNumChar <|> char '_')

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

definitionReferenceP :: [Import] -> [TypeVariable] -> Parser DefinitionReference
definitionReferenceP imports typeVariables = do
  definitions <- getDefinitions
  let definitionNames =
        definitions
          & List.map (\(TypeDefinition (DefinitionName n) _typeData) -> n)
          & List.sortBy (\n1 n2 -> compare (Text.length n2) (Text.length n1))
  soughtName@(DefinitionName n) <- DefinitionName <$> choice (List.map string definitionNames)
  maybeDefinition <- getDefinition soughtName
  maybeTypeVariables <-
    optional $ angleBracketed $ appliedTypeVariablesP imports typeVariables
  ensureMatchingGenericity Nothing maybeDefinition maybeTypeVariables
  case maybeDefinition of
    Just definition@(TypeDefinition name' (DeclaredType moduleName' _typeVariables)) ->
      case maybeTypeVariables of
        Nothing ->
          pure $ DeclarationReference moduleName' name'
        Just appliedTypes ->
          if isGenericType definition
            then pure $ GenericDeclarationReference moduleName' name' (AppliedTypes appliedTypes)
            else
              reportError $
                mconcat ["Trying to apply type as generic, but ", unpack n, " is not generic"]
    Just definition -> do
      case maybeTypeVariables of
        Just appliedTypeVariables ->
          if isGenericType definition
            then pure $ AppliedGenericReference appliedTypeVariables definition
            else
              reportError $
                mconcat ["Trying to apply type as generic, but ", unpack n, " is not generic"]
        Nothing ->
          pure $ DefinitionReference definition
    Nothing -> reportError $ mconcat ["Unknown type reference: ", unpack n]

ensureMatchingGenericity :: Maybe ModuleName -> Maybe TypeDefinition -> Maybe [FieldType] -> Parser ()
ensureMatchingGenericity _maybeModuleName Nothing _maybeTypeParameters = pure ()
ensureMatchingGenericity maybeModuleName (Just definition) maybeTypeParameters = do
  let expectedTypeParameters =
        definition
          & Utilities.typeVariablesFrom
          & fromMaybe []
          & length
      name =
        mconcat
          [ maybe "" (unModuleName >>> (<> ".") >>> unpack) maybeModuleName,
            definition ^. typeDefinitionName . unwrap & unpack
          ]
      appliedTypeParameters = maybeTypeParameters & fromMaybe [] & length
  if expectedTypeParameters /= appliedTypeParameters
    then
      reportError $
        mconcat
          [ "Type '",
            name,
            "' expects ",
            show expectedTypeParameters,
            " type parameters, ",
            show appliedTypeParameters,
            " applied"
          ]
    else pure ()

appliedTypeVariablesP :: [Import] -> [TypeVariable] -> Parser [FieldType]
appliedTypeVariablesP imports typeVariables =
  sepBy1 (fieldTypeP imports typeVariables) (string ", ")

fieldTypeP :: [Import] -> [TypeVariable] -> Parser FieldType
fieldTypeP imports typeVariables =
  choice
    [ LiteralType <$> literalP,
      ComplexType <$> complexTypeP imports typeVariables,
      TypeVariableReferenceType <$> typeVariableReferenceP typeVariables,
      DefinitionReferenceType <$> definitionReferenceP imports typeVariables,
      BasicType <$> basicTypeValueP,
      DefinitionReferenceType <$> importedReferenceP imports typeVariables,
      RecursiveReferenceType <$> recursiveReferenceP
    ]
    <* many (char ' ')

nameAndMaybeTypeVariablesP :: Parser (DefinitionName, Maybe [Text])
nameAndMaybeTypeVariablesP = do
  name <- lexeme readCurrentDefinitionName
  maybeTypeVariables <- optional $ angleBracketed typeVariablesP
  pure (name, maybeTypeVariables)

typeVariableReferenceP :: [TypeVariable] -> Parser TypeVariable
typeVariableReferenceP typeVariables =
  TypeVariable <$> choice (List.map (\(TypeVariable t) -> string t) typeVariables)

importedReferenceP :: [Import] -> [TypeVariable] -> Parser DefinitionReference
importedReferenceP imports typeVariables = do
  moduleName' <-
    choice
      (((^. unwrap . moduleName . unwrap) >>> string) <$> imports)
      <* char '.'

  definitionName <- definitionNameP
  case findImport moduleName' imports of
    Just import' -> do
      case List.find
        (\d -> d ^. typeDefinitionName . unwrap == definitionName ^. unwrap)
        (import' ^. unwrap . moduleDefinitions) of
        Just definition@(TypeDefinition foundDefinitionName typeData) -> do
          maybeTypeVariables <-
            optional $ angleBracketed $ appliedTypeVariablesP imports typeVariables
          ensureMatchingGenericity
            (Just $ ModuleName moduleName')
            (Just definition)
            maybeTypeVariables
          pure $ case maybeTypeVariables of
            Just appliedTypeVariables ->
              AppliedImportedGenericReference
                (ModuleName moduleName')
                (AppliedTypes appliedTypeVariables)
                definition
            Nothing ->
              ImportedDefinitionReference (import' ^. unwrap . moduleName) $
                TypeDefinition foundDefinitionName typeData
        Nothing ->
          reportError $
            mconcat
              [ "Unknown definition in module '",
                unpack moduleName',
                "': ",
                definitionName ^. unwrap & unpack
              ]
    Nothing ->
      reportError $ "Unknown module referenced, not in imports: " <> unpack moduleName'

basicTypeValueP :: Parser BasicTypeValue
basicTypeValueP = choice [uintP, intP, floatP, booleanP, basicStringP]

complexTypeP :: [Import] -> [TypeVariable] -> Parser ComplexTypeValue
complexTypeP imports typeVariables =
  choice
    [ sliceTypeP imports typeVariables,
      arrayTypeP imports typeVariables,
      optionalTypeP imports typeVariables,
      pointerTypeP imports typeVariables
    ]

sliceTypeP :: [Import] -> [TypeVariable] -> Parser ComplexTypeValue
sliceTypeP imports typeVariables = SliceType <$> (string "[]" *> fieldTypeP imports typeVariables)

arrayTypeP :: [Import] -> [TypeVariable] -> Parser ComplexTypeValue
arrayTypeP imports typeVariables = do
  size <- between (char '[') (char ']') decimal
  ArrayType size <$> fieldTypeP imports typeVariables

optionalTypeP :: [Import] -> [TypeVariable] -> Parser ComplexTypeValue
optionalTypeP imports typeVariables =
  OptionalType <$> (char '?' *> fieldTypeP imports typeVariables)

pointerTypeP :: [Import] -> [TypeVariable] -> Parser ComplexTypeValue
pointerTypeP imports typeVariables = PointerType <$> (char '*' *> fieldTypeP imports typeVariables)

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
  floatType <- choice ["F32", "F64"]
  case floatType of
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
literalStringP = (pack >>> LiteralString) <$> quoted printChar

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

isGenericType :: TypeDefinition -> Bool
isGenericType (TypeDefinition _name (Struct (GenericStruct _typeVariables _fields))) = True
isGenericType (TypeDefinition _name (Union _tag (GenericUnion _typeVariables _constructors))) = True
isGenericType (TypeDefinition _name (Struct (PlainStruct _fields))) = False
isGenericType (TypeDefinition _name (Union _tag (PlainUnion _constructors))) = False
isGenericType (TypeDefinition _name (DeclaredType _moduleName typeVariables)) =
  not $ List.null typeVariables
isGenericType (TypeDefinition _name (EmbeddedUnion _tag _constructors)) = False
isGenericType (TypeDefinition _name (UntaggedUnion _cases)) = False
isGenericType (TypeDefinition _name (Enumeration _values)) = False

definitionNameP :: Parser DefinitionName
definitionNameP = do
  initialTitleCaseCharacter <- upperChar
  ((initialTitleCaseCharacter :) >>> pack >>> DefinitionName) <$> many alphaNumChar

addDeclarationName :: ModuleName -> Parser ()
addDeclarationName moduleName' = do
  AppState {currentDeclarationNamesReference} <- ask
  modifyIORef currentDeclarationNamesReference (Set.insert moduleName')

getDeclarationNames :: Parser (Set ModuleName)
getDeclarationNames = do
  AppState {currentDeclarationNamesReference} <- ask
  readIORef currentDeclarationNamesReference

getModule :: String -> Parser (Maybe Module)
getModule importName = do
  AppState {modulesReference} <- ask
  modules <- readIORef modulesReference
  pure $ List.find (\module' -> module' ^. moduleName . unwrap == pack importName) modules

addModule :: Module -> IORef [Module] -> IO ()
addModule module' modulesReference = do
  modifyIORef modulesReference (module' :)

readCurrentDefinitionName :: Parser DefinitionName
readCurrentDefinitionName = do
  name <- definitionNameP
  setCurrentDefinitionName name
  pure name

setCurrentDefinitionName :: DefinitionName -> Parser ()
setCurrentDefinitionName name = do
  AppState {currentDefinitionNameReference} <- ask
  writeIORef currentDefinitionNameReference (Just name)

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

findImport :: Text -> [Import] -> Maybe Import
findImport soughtName =
  List.find (\import' -> soughtName == import' ^. unwrap . moduleName . unwrap)

reportError :: String -> Parser a
reportError = ErrorFail >>> Set.singleton >>> fancyFailure

integerSizes :: [Int]
integerSizes = [8, 16, 32, 64, 128]

integerTypeParsers :: Text -> [Parser Text]
integerTypeParsers prefix = List.map (show >>> pack >>> (prefix <>) >>> string) integerSizes

partialFromRight :: Either l r -> r
partialFromRight (Right r) = r
partialFromRight (Left _l) = error "Unable to get `Right` from `Left`"

partialFromLeft :: Either l r -> l
partialFromLeft (Left l) = l
partialFromLeft (Right _r) = error "Unable to get `Left` from `Right`"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 (Lexer.skipLineComment "# ") empty

angleBracketed :: Parser a -> Parser a
angleBracketed = between (char '<') (char '>')

quoted :: Parser a -> Parser [a]
quoted p = char '"' *> manyTill p (char '"')
