module Types where

import RIO

data Expression
  = ImportExpression Import
  | TypeDefinitionExpression TypeDefinition
  deriving (Eq, Show)

newtype Import = Import {unImport :: Text}
  deriving (Eq, Show)

newtype DefinitionName = DefinitionName {unDefinitionName :: Text}
  deriving (Eq, Show)

data TypeDefinition = TypeDefinition
  { name :: DefinitionName,
    typeData :: TypeData
  }
  deriving (Eq, Show)

newtype TypeData
  = PlainStruct PlainStructData
  deriving (Eq, Show)

data PlainStructData = PlainStructData
  { name :: Text,
    fields :: [StructField]
  }
  deriving (Eq, Show)

data StructField = StructField
  { name :: Text,
    fieldType :: FieldType
  }
  deriving (Eq, Show)

data FieldType
  = LiteralType LiteralTypeValue
  | BasicType BasicTypeValue
  | DefinitionReferenceType TypeDefinition
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

data LiteralTypeValue
  = LiteralString Text
  | LiteralInteger Integer
  | LiteralFloat Float
  | LiteralBoolean Bool
  deriving (Eq, Show)
