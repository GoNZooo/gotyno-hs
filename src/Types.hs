module Types where

import RIO

data Module = Module
  { name :: !ModuleName,
    imports :: ![Import],
    declarationNames :: ![ModuleName],
    definitions :: ![TypeDefinition],
    sourceFile :: !FilePath
  }
  deriving (Eq, Show)

newtype ModuleName = ModuleName Text
  deriving (Eq, Show, Ord)

newtype Import = Import Module
  deriving (Eq, Show)

newtype DefinitionName = DefinitionName Text
  deriving (Eq, Show)

data TypeDefinition = TypeDefinition !DefinitionName !TypeData
  deriving (Eq, Show)

data ImportedTypeDefinition = ImportedTypeDefinition
  { sourceModule :: !ModuleName,
    name :: !DefinitionName,
    typeData :: !TypeData
  }
  deriving (Eq, Show)

newtype TypeTag = TypeTag Text
  deriving (Eq, Show)

newtype TypeVariable = TypeVariable Text
  deriving (Eq, Show)

newtype ConstructorName = ConstructorName Text
  deriving (Eq, Show)

newtype FieldName = FieldName Text
  deriving (Eq, Show)

newtype EnumerationIdentifier = EnumerationIdentifier Text
  deriving (Eq, Show)

-- | Defines what type tag field a union should have as well as the type tag location.
data TagType
  = -- | The union is untagged.
    NoTypeTag
  | -- | The union has the type tag with the rest of the payload.
    EmbeddedTypeTag FieldName
  | -- | The union has the type tag outside of the payload, wrapping it.
    StandardTypeTag FieldName
  deriving (Eq, Show)

data TypeData
  = Struct !StructType
  | Union !FieldName !UnionType
  | EmbeddedUnion !FieldName ![EmbeddedConstructor]
  | UntaggedUnion ![FieldType]
  | Enumeration ![EnumerationValue]
  | DeclaredType !ModuleName ![TypeVariable]
  deriving (Eq, Show)

data EmbeddedConstructor = EmbeddedConstructor !ConstructorName !DefinitionReference
  deriving (Eq, Show)

data StructType
  = PlainStruct ![StructField]
  | GenericStruct ![TypeVariable] ![StructField]
  deriving (Eq, Show)

data UnionType
  = PlainUnion ![Constructor]
  | GenericUnion ![TypeVariable] ![Constructor]
  deriving (Eq, Show)

data Constructor = Constructor !ConstructorName !(Maybe FieldType)
  deriving (Eq, Show)

data StructField = StructField !FieldName !FieldType
  deriving (Eq, Show)

data EnumerationValue = EnumerationValue !EnumerationIdentifier !LiteralTypeValue
  deriving (Eq, Show)

data FieldType
  = LiteralType !LiteralTypeValue
  | BasicType !BasicTypeValue
  | ComplexType !ComplexTypeValue
  | DefinitionReferenceType !DefinitionReference
  | RecursiveReferenceType !DefinitionName
  | TypeVariableReferenceType !TypeVariable
  deriving (Eq, Show)

data DefinitionReference
  = DefinitionReference !TypeDefinition
  | ImportedDefinitionReference !ModuleName !TypeDefinition
  | AppliedGenericReference ![FieldType] !TypeDefinition
  | AppliedImportedGenericReference !ModuleName !AppliedTypes !TypeDefinition
  | DeclarationReference !ModuleName !DefinitionName
  | GenericDeclarationReference !ModuleName !DefinitionName !AppliedTypes
  deriving (Eq, Show)

newtype AppliedTypes = AppliedTypes [FieldType]
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data ComplexTypeValue
  = SliceType FieldType
  | ArrayType Integer FieldType
  | OptionalType FieldType
  | PointerType FieldType
  deriving (Eq, Show)

data LiteralTypeValue
  = LiteralString !Text
  | LiteralInteger !Integer
  | LiteralFloat !Float
  | LiteralBoolean !Bool
  deriving (Eq, Show)
