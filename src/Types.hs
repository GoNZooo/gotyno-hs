module Types where

import RIO

data Module = Module
  { name :: !Text,
    imports :: [Import],
    definitions :: [TypeDefinition]
  }
  deriving (Eq, Show)

newtype Import = Import Module
  deriving (Eq, Show)

newtype DefinitionName = DefinitionName Text
  deriving (Eq, Show)

data TypeDefinition = TypeDefinition
  { name :: !DefinitionName,
    typeData :: !TypeData
  }
  deriving (Eq, Show)

data ImportedTypeDefinition = ImportedTypeDefinition
  { sourceModule :: !Text,
    name :: !DefinitionName,
    typeData :: !TypeData
  }
  deriving (Eq, Show)

data TypeData
  = PlainStruct PlainStructData
  | GenericStruct GenericStructData
  | PlainUnion PlainUnionData
  | GenericUnion GenericUnionData
  | Enumeration EnumerationData
  deriving (Eq, Show)

newtype PlainStructData = PlainStructData
  { fields :: [StructField]
  }
  deriving (Eq, Show)

data GenericStructData = GenericStructData
  { fields :: [StructField],
    typeVariables :: [Text]
  }
  deriving (Eq, Show)

newtype PlainUnionData = PlainUnionData
  { constructors :: [Constructor]
  }
  deriving (Eq, Show)

data GenericUnionData = GenericUnionData
  { constructors :: [Constructor],
    typeVariables :: [Text]
  }
  deriving (Eq, Show)

data Constructor = Constructor
  { name :: !Text,
    payload :: !(Maybe FieldType)
  }
  deriving (Eq, Show)

data StructField = StructField
  { name :: Text,
    fieldType :: FieldType
  }
  deriving (Eq, Show)

newtype EnumerationData = EnumerationData
  { values :: [EnumerationValue]
  }
  deriving (Eq, Show)

data EnumerationValue = EnumerationValue
  { identifier :: Text,
    value :: LiteralTypeValue
  }
  deriving (Eq, Show)

data FieldType
  = LiteralType LiteralTypeValue
  | BasicType BasicTypeValue
  | ComplexType ComplexTypeValue
  | DefinitionReferenceType DefinitionReference
  | RecursiveReferenceType DefinitionName
  | TypeVariableReferenceType Text
  deriving (Eq, Show)

data DefinitionReference
  = DefinitionReference TypeDefinition
  | ImportedDefinitionReference ImportedTypeDefinition
  | AppliedGenericReference [FieldType] TypeDefinition
  | AppliedImportedGenericReference AppliedImportedGenericReferenceData
  deriving (Eq, Show)

data AppliedImportedGenericReferenceData = AppliedImportedGenericReferenceData
  { moduleName :: !Text,
    typeVariables :: ![FieldType],
    definition :: !TypeDefinition
  }
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
  = LiteralString Text
  | LiteralInteger Integer
  | LiteralFloat Float
  | LiteralBoolean Bool
  deriving (Eq, Show)
