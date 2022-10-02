{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Qtility
import RIO.Time (NominalDiffTime)
import Text.Megaparsec (ParsecT)

-- | Represents the compiler state at any given moment in compilation.
data AppState = AppState
  { _modulesReference :: IORef [Module],
    _currentDeclarationNamesReference :: IORef (Set ModuleName),
    _currentDefinitionsReference :: IORef [TypeDefinition],
    _currentDefinitionNameReference :: IORef (Maybe DefinitionName)
  }

type Parser = ParsecT Void Text (RIO AppState)

-- | Where to write compiler output to.
data OutputDestination
  = -- | Write to the same folder as the input files, no matter whether they are in the same
    -- folder or not.
    SameAsInput
  | -- | Write to a specified folder.
    OutputPath FilePath
  | -- | Write to standard out (@stdout@).
    StandardOut
  deriving (Eq, Show, Generic)

data Languages = Languages
  { _languagesTypescript :: Maybe OutputDestination,
    _languagesFsharp :: Maybe OutputDestination,
    _languagesPython :: Maybe OutputDestination,
    _languagesHaskell :: Maybe OutputDestination,
    _languagesKotlin :: Maybe OutputDestination
  }
  deriving (Eq, Show, Generic)

data Options = Options
  { _optionsLanguages :: Languages,
    _optionsWatchMode :: Bool,
    _optionsVerbose :: Bool,
    _optionsInputs :: [FilePath]
  }
  deriving (Eq, Show, Generic)

-- | A parsed module which should contain everything we need to output to a language.
data Module = Module
  { _moduleName :: ModuleName,
    _moduleImports :: [Import],
    _moduleDeclarationNames :: [ModuleName],
    _moduleDefinitions :: [TypeDefinition],
    _moduleSourceFile :: FilePath
  }
  deriving (Eq, Show, Generic)

newtype ModuleName = ModuleName {unModuleName :: Text}
  deriving (Eq, Show, Ord, Generic)

newtype Import = Import {unImport :: Module}
  deriving (Eq, Show, Generic)

newtype DefinitionName = DefinitionName {unDefinitionName :: Text}
  deriving (Eq, Show, Generic)

data TypeDefinition = TypeDefinition
  { _typeDefinitionName :: DefinitionName,
    _typeDefinitionType :: TypeData
  }
  deriving (Eq, Show, Generic)

data ImportedTypeDefinition = ImportedTypeDefinition
  { _importedTypeDefinitionSourceModule :: ModuleName,
    _importedTypeDefinitionName :: DefinitionName,
    _importedTypeDefinitionTypeData :: TypeData
  }
  deriving (Eq, Show, Generic)

newtype TypeTag = TypeTag {unTypeTag :: Text}
  deriving (Eq, Show, Generic)

newtype TypeVariable = TypeVariable {unTypeVariable :: Text}
  deriving (Eq, Show, Generic)

newtype ConstructorName = ConstructorName {unConstructorName :: Text}
  deriving (Eq, Show, Generic)

newtype FieldName = FieldName {unFieldName :: Text}
  deriving (Eq, Show, Generic)

newtype EnumerationIdentifier = EnumerationIdentifier {unEnumerationIdentifier :: Text}
  deriving (Eq, Show, Generic)

-- | Defines what type tag field a union should have as well as the type tag location.
data TagType
  = -- | The union has the type tag with the rest of the payload.
    EmbeddedTypeTag FieldName
  | -- | The union has the type tag outside of the payload, wrapping it.
    StandardTypeTag FieldName
  deriving (Eq, Show, Generic)

data TypeData
  = Struct StructType
  | Union FieldName UnionType
  | EmbeddedUnion FieldName [EmbeddedConstructor]
  | UntaggedUnion [FieldType]
  | Enumeration BasicTypeValue [EnumerationValue]
  | DeclaredType ModuleName [TypeVariable]
  deriving (Eq, Show, Generic)

data EmbeddedConstructor = EmbeddedConstructor
  { _embeddedConstructorName :: ConstructorName,
    _embeddedConstructorPayload :: Maybe DefinitionReference
  }
  deriving (Eq, Show, Generic)

data StructType
  = PlainStruct [StructField]
  | GenericStruct [TypeVariable] [StructField]
  deriving (Eq, Show, Generic)

data UnionType
  = PlainUnion [Constructor]
  | GenericUnion [TypeVariable] [Constructor]
  deriving (Eq, Show, Generic)

data Constructor = Constructor
  { _constructorName :: ConstructorName,
    _constructorPayloadType :: Maybe FieldType
  }
  deriving (Eq, Show, Generic)

data StructField = StructField
  { _structFieldName :: FieldName,
    _structFieldType :: FieldType
  }
  deriving (Eq, Show, Generic)

data EnumerationValue = EnumerationValue
  { _enumerationValueIdentifier :: EnumerationIdentifier,
    _enumerationValueValue :: LiteralTypeValue
  }
  deriving (Eq, Show, Generic)

data FieldType
  = LiteralType LiteralTypeValue
  | BasicType BasicTypeValue
  | ComplexType ComplexTypeValue
  | DefinitionReferenceType DefinitionReference
  | RecursiveReferenceType DefinitionName
  | TypeVariableReferenceType TypeVariable
  deriving (Eq, Show, Generic)

data DefinitionReference
  = DefinitionReference TypeDefinition
  | ImportedDefinitionReference ModuleName TypeDefinition
  | AppliedGenericReference [FieldType] TypeDefinition
  | AppliedImportedGenericReference ModuleName AppliedTypes TypeDefinition
  | DeclarationReference ModuleName DefinitionName
  | GenericDeclarationReference ModuleName DefinitionName AppliedTypes
  deriving (Eq, Show, Generic)

newtype AppliedTypes = AppliedTypes {unAppliedTypes :: [FieldType]}
  deriving (Eq, Show, Generic)

data BasicTypeValue
  = U8
  | U16
  | U32
  | U64
  | U128
  | I8
  | I16
  | I32
  | I64
  | I128
  | F32
  | F64
  | Boolean
  | BasicString
  deriving (Eq, Show, Generic)

data ComplexTypeValue
  = SliceType FieldType
  | ArrayType Integer FieldType
  | OptionalType FieldType
  | PointerType FieldType
  deriving (Eq, Show, Generic)

data LiteralTypeValue
  = LiteralString Text
  | LiteralInteger Integer
  | LiteralFloat Float
  | LiteralBoolean Bool
  deriving (Eq, Show, Generic)

data CompilationState = CompilationState
  { _compilationStateState :: Either FailedCompilation SuccessfulCompilation,
    _compilationStateOptions :: Options
  }
  deriving (Eq, Show, Generic)

data SuccessfulCompilation = SuccessfulCompilation
  { _successfulCompilationTotalTime :: NominalDiffTime,
    _successfulCompilationParsingTime :: NominalDiffTime,
    _successfulCompilationModuleStatistics :: [ModuleStatistics],
    _successfulCompilationLanguageTimes :: LanguageOutputStatistics
  }
  deriving (Eq, Show, Generic)

data ModuleStatistics = ModuleStatistics
  { _moduleStatisticsName :: Text,
    _moduleStatisticsPath :: FilePath,
    _moduleStatisticsTime :: NominalDiffTime,
    _moduleStatisticsLanguage :: OutputLanguage
  }
  deriving (Eq, Show, Generic)

newtype FailedCompilation = FailedCompilation {unFailedCompilation :: [String]}
  deriving (Eq, Show, Generic)

data OutputLanguage
  = FSharpOutput
  | PythonOutput
  | TypeScriptOutput
  | HaskellOutput
  | KotlinOutput
  deriving (Eq, Show, Generic)

data LanguageOutputStatistics = LanguageOutputStatistics
  { _languageOutputStatisticsFsharpTime :: Maybe NominalDiffTime,
    _languageOutputStatisticsPythonTime :: Maybe NominalDiffTime,
    _languageOutputStatisticsTypescriptTime :: Maybe NominalDiffTime,
    _languageOutputStatisticsHaskellTime :: Maybe NominalDiffTime,
    _languageOutputStatisticsKotlinTime :: Maybe NominalDiffTime
  }
  deriving (Eq, Show, Generic)

foldMapM
  makeWrapped
  [ ''ModuleName,
    ''Import,
    ''DefinitionName,
    ''TypeTag,
    ''TypeVariable,
    ''ConstructorName,
    ''FieldName,
    ''EnumerationIdentifier,
    ''AppliedTypes,
    ''FailedCompilation
  ]

foldMapM
  makeLenses
  [ ''Options,
    ''Module,
    ''TypeDefinition,
    ''ImportedTypeDefinition,
    ''TypeData,
    ''EmbeddedConstructor,
    ''StructType,
    ''UnionType,
    ''Constructor,
    ''StructField,
    ''EnumerationValue,
    ''FieldType,
    ''DefinitionReference,
    ''AppliedTypes,
    ''CompilationState,
    ''SuccessfulCompilation,
    ''ModuleStatistics,
    ''FailedCompilation,
    ''LanguageOutputStatistics,
    ''Languages,
    ''AppState
  ]
