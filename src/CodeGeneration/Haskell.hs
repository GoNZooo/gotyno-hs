module CodeGeneration.Haskell (outputModule) where

import CodeGeneration.Utilities (lowerCaseFirstCharacter, upperCaseFirstCharacter)
import RIO
import qualified RIO.Text as Text
import Types

outputModule :: Module -> Text
outputModule Module {name = ModuleName name, definitions, imports, declarationNames} =
  let definitionOutput = definitions & mapMaybe outputDefinition & Text.intercalate "\n\n"
      importsOutput = imports & fmap outputImport & Text.intercalate "\n"
      outputImport (Import Module {name = ModuleName importName}) =
        mconcat
          [ "import qualified GotynoOutput.",
            haskellifyModuleName importName,
            " as ",
            haskellifyModuleName importName
          ]
      declarationImportsOutput =
        declarationNames
          & fmap
            ( \(ModuleName declarationModuleName) ->
                mconcat ["import qualified ", haskellifyModuleName declarationModuleName]
            )
          & Text.intercalate "\n"
   in mconcat
        [ modulePrelude (haskellifyModuleName name),
          "\n",
          if Text.null importsOutput then "" else importsOutput <> "\n\n",
          if Text.null declarationImportsOutput then "" else declarationImportsOutput <> "\n\n",
          definitionOutput
        ]

haskellifyModuleName :: Text -> Text
haskellifyModuleName = upperCaseFirstCharacter

modulePrelude :: Text -> Text
modulePrelude name =
  Text.unlines
    [ "{-# LANGUAGE StrictData #-}",
      "{-# LANGUAGE TemplateHaskell #-}",
      "",
      mconcat ["module GotynoOutput.", name, " where"],
      "",
      "import Data.Aeson (FromJSON (..), ToJSON (..))",
      "import qualified Data.Aeson as JSON",
      "import GHC.Generics (Generic)",
      "import qualified Gotyno.Helpers as Helpers",
      "import Qtility"
    ]

outputDefinition :: TypeDefinition -> Maybe Text
outputDefinition (TypeDefinition name (Struct (PlainStruct fields))) =
  pure $ outputPlainStruct name fields
outputDefinition (TypeDefinition name (Struct (GenericStruct typeVariables fields))) =
  pure $ outputGenericStruct name typeVariables fields
outputDefinition (TypeDefinition name (Union typeTag unionType)) =
  pure $ outputUnion name typeTag unionType
outputDefinition (TypeDefinition name (Enumeration enumerationValues)) =
  pure $ outputEnumeration name enumerationValues
outputDefinition (TypeDefinition name (UntaggedUnion unionCases)) =
  pure $ outputUntaggedUnion name unionCases
outputDefinition (TypeDefinition name (EmbeddedUnion typeTag constructors)) =
  pure $ outputEmbeddedUnion name typeTag constructors
outputDefinition (TypeDefinition _name (DeclaredType _moduleName _typeVariables)) = Nothing

outputEmbeddedUnion :: DefinitionName -> FieldName -> [EmbeddedConstructor] -> Text
outputEmbeddedUnion unionName (FieldName tag) constructors =
  let typeOutput =
        outputCaseUnion
          unionName
          constructorsAsConstructors
          []
      constructorsAsConstructors = embeddedConstructorsToConstructors constructors
      toJsonOutput =
        mconcat
          [ "instance ToJSON ",
            unDefinitionName unionName,
            " where",
            "\n  ",
            toJsonCaseOutput
          ]
      toJsonCaseOutput =
        constructors
          & fmap constructorToJson
          & Text.intercalate "\n  "
      constructorToJson (EmbeddedConstructor (ConstructorName n) (Just _payload)) =
        mconcat
          [ "toJSON (",
            n,
            " payload) = toJSON payload & atKey \"",
            tag,
            "\" ?~ String \"",
            n,
            "\""
          ]
      constructorToJson (EmbeddedConstructor (ConstructorName n) Nothing) =
        mconcat
          [ "toJSON ",
            n,
            " = object [] & atKey \"",
            tag,
            "\" ?~ String \"",
            n,
            "\""
          ]
      fromJsonOutput =
        mconcat
          [ "instance FromJSON ",
            unDefinitionName unionName,
            " where",
            "\n  parseJSON = withObject \"",
            unDefinitionName unionName,
            "\" $ \\o -> do\n",
            "    t :: Text <- o .: \"",
            tag,
            "\"\n",
            "    case t of\n      ",
            fromJsonCaseOutput,
            "\n      tagValue -> fail $ \"Invalid type tag: \" <> show tagValue"
          ]
      fromJsonCaseOutput =
        constructors
          & fmap constructorFromJson
          & Text.intercalate "\n      "
      constructorFromJson (EmbeddedConstructor (ConstructorName n) (Just _payload)) =
        mconcat ["\"", n, "\" -> ", n, " <$> parseJSON (Object o)"]
      constructorFromJson (EmbeddedConstructor (ConstructorName n) Nothing) =
        mconcat ["\"", n, "\" -> pure ", n]
   in mconcat
        [ typeOutput,
          "\n",
          "  deriving (Eq, Show, Generic)",
          "\n\n",
          toJsonOutput,
          "\n\n",
          fromJsonOutput
        ]

embeddedConstructorsToConstructors :: [EmbeddedConstructor] -> [Constructor]
embeddedConstructorsToConstructors = fmap embeddedConstructorToConstructor

embeddedConstructorToConstructor :: EmbeddedConstructor -> Constructor
embeddedConstructorToConstructor (EmbeddedConstructor name reference) =
  Constructor name (DefinitionReferenceType <$> reference)

outputUntaggedUnion :: DefinitionName -> [FieldType] -> Text
outputUntaggedUnion unionName cases =
  let typeOutput = mconcat ["data ", unDefinitionName unionName, "\n  = ", unionOutput]
      deriveLensAndJSONOutput =
        mconcat ["deriveLensAndJSON' 'Helpers.untaggedUnionOptions ''", unDefinitionName unionName]
      unionOutput = cases & fmap outputCaseLine & Text.intercalate "\n  | "
      outputCaseLine fieldType =
        mconcat
          [ unDefinitionName unionName,
            fieldTypeName fieldType,
            " ",
            outputFieldType fieldType
          ]
   in mconcat
        [ typeOutput,
          "\n",
          "  deriving (Eq, Show, Generic)",
          "\n\n",
          deriveLensAndJSONOutput
        ]

outputEnumeration :: DefinitionName -> [EnumerationValue] -> Text
outputEnumeration name values =
  let typeOutput = outputEnumerationType name values
   in mconcat [typeOutput]

outputEnumerationType :: DefinitionName -> [EnumerationValue] -> Text
outputEnumerationType name values =
  let valuesOutput =
        values
          & fmap
            ( \(EnumerationValue (EnumerationIdentifier i) _literal) ->
                mconcat ["    | ", haskellifyConstructorName i]
            )
          & Text.intercalate "\n"
   in mconcat [mconcat ["type ", unDefinitionName name, " =\n"], valuesOutput]

haskellifyConstructorName :: Text -> Text
haskellifyConstructorName = upperCaseFirstCharacter

outputPlainStruct :: DefinitionName -> [StructField] -> Text
outputPlainStruct name fields =
  let fieldsOutput = fields & fmap (outputField name) & Text.intercalate ",\n    "
      deriveLensAndJSONOutput = mconcat ["deriveLensAndJSON ''", unDefinitionName name]
   in mconcat
        [ mconcat ["data ", unDefinitionName name, " = ", unDefinitionName name, "\n"],
          "  { ",
          fieldsOutput,
          "\n  }",
          "\n",
          "  deriving (Eq, Show, Generic)",
          "\n\n",
          deriveLensAndJSONOutput
        ]

outputGenericStruct :: DefinitionName -> [TypeVariable] -> [StructField] -> Text
outputGenericStruct name typeVariables fields =
  let fullName = unDefinitionName name <> joinTypeVariables typeVariables
      deriveLensAndJSONOutput = mconcat ["deriveLensAndJSON ''", unDefinitionName name]
      fieldsOutput = fields & fmap (outputField name) & Text.intercalate ",\n    "
   in mconcat
        [ mconcat ["data ", fullName, " = ", unDefinitionName name, "\n"],
          "  { ",
          fieldsOutput,
          "\n  }",
          "\n",
          "  deriving (Eq, Show, Generic)",
          "\n\n",
          deriveLensAndJSONOutput
        ]

outputUnion :: DefinitionName -> FieldName -> UnionType -> Text
outputUnion name typeTag unionType =
  let caseUnionOutput = outputCaseUnion name (constructorsFrom unionType) typeVariables
      constructorsFrom (PlainUnion constructors) = constructors
      constructorsFrom (GenericUnion _typeVariables constructors) = constructors
      typeVariables = case unionType of
        PlainUnion _constructors -> []
        GenericUnion ts _constructors -> ts
      lowerCasedTypeVariables = fmap (unTypeVariable >>> lowerCaseFirstCharacter) typeVariables
      toJsonHeader =
        if null typeVariables
          then mconcat ["instance ToJSON ", unDefinitionName name, " where"]
          else
            mconcat
              [ "instance (",
                lowerCasedTypeVariables & fmap ("ToJSON " <>) & Text.intercalate ", ",
                ") => ToJSON (",
                unDefinitionName name,
                " ",
                Text.intercalate " " lowerCasedTypeVariables,
                ") where"
              ]
      fromJsonHeader =
        if null typeVariables
          then mconcat ["instance FromJSON ", unDefinitionName name, " where"]
          else
            mconcat
              [ "instance (",
                lowerCasedTypeVariables & fmap ("FromJSON " <>) & Text.intercalate ", ",
                ") => FromJSON (",
                unDefinitionName name,
                " ",
                Text.intercalate " " lowerCasedTypeVariables,
                ") where"
              ]
      toJsonOutput =
        Text.intercalate
          "\n"
          [ toJsonHeader,
            mconcat
              [ "  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions \"",
                unFieldName typeTag,
                "\""
              ]
          ]
      fromJsonOutput =
        Text.intercalate
          "\n"
          [ fromJsonHeader,
            mconcat
              [ "  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions \"",
                unFieldName typeTag,
                "\""
              ]
          ]
   in mconcat
        [ caseUnionOutput,
          "\n",
          "  deriving (Eq, Show, Generic)",
          "\n\n",
          toJsonOutput,
          "\n\n",
          fromJsonOutput
        ]

outputCaseUnion :: DefinitionName -> [Constructor] -> [TypeVariable] -> Text
outputCaseUnion name constructors typeVariables =
  let cases =
        constructors
          & fmap outputCaseConstructor
          & Text.intercalate "\n  | "
      maybeTypeVariables = if null typeVariables then "" else joinTypeVariables typeVariables
   in mconcat
        [ mconcat ["data ", unDefinitionName name, maybeTypeVariables, "\n  = "],
          cases
        ]
  where
    outputCaseConstructor (Constructor (ConstructorName constructorName) Nothing) =
      upperCaseFirstCharacter constructorName
    outputCaseConstructor (Constructor (ConstructorName constructorName) (Just payload)) =
      mconcat [upperCaseFirstCharacter constructorName, " ", outputFieldType payload]

outputField :: DefinitionName -> StructField -> Text
outputField definitionName (StructField fieldName fieldType) =
  mconcat
    [ recordFieldName definitionName fieldName,
      " :: ",
      outputFieldType fieldType
    ]

recordFieldName :: DefinitionName -> FieldName -> Text
recordFieldName (DefinitionName name) (FieldName fieldName) =
  mconcat ["_", lowerCaseFirstCharacter name, upperCaseFirstCharacter fieldName]

outputFieldType :: FieldType -> Text
outputFieldType (LiteralType (LiteralString _text)) = outputBasicType BasicString
outputFieldType (LiteralType (LiteralInteger _x)) = outputBasicType I32
outputFieldType (LiteralType (LiteralFloat _f)) = outputBasicType F32
outputFieldType (LiteralType (LiteralBoolean _b)) = outputBasicType Boolean
outputFieldType (BasicType basicType) = outputBasicType basicType
outputFieldType (ComplexType (OptionalType fieldType)) =
  mconcat ["(Maybe ", outputFieldType fieldType, ")"]
outputFieldType (ComplexType (ArrayType _size fieldType)) =
  mconcat ["[", outputFieldType fieldType, "]"]
outputFieldType (ComplexType (SliceType fieldType)) =
  mconcat ["[", outputFieldType fieldType, "]"]
outputFieldType (ComplexType (PointerType fieldType)) = outputFieldType fieldType
outputFieldType (RecursiveReferenceType (DefinitionName name)) = name
outputFieldType (DefinitionReferenceType definitionReference) =
  outputDefinitionReference definitionReference
outputFieldType (TypeVariableReferenceType (TypeVariable t)) = haskellifyTypeVariable t

haskellifyTypeVariable :: Text -> Text
haskellifyTypeVariable = Text.toLower

outputDefinitionReference :: DefinitionReference -> Text
outputDefinitionReference (DefinitionReference (TypeDefinition (DefinitionName name) _)) = name
outputDefinitionReference
  ( ImportedDefinitionReference
      (ModuleName moduleName)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    mconcat [haskellifyModuleName moduleName, ".", name]
outputDefinitionReference
  ( AppliedGenericReference
      appliedTypes
      (TypeDefinition (DefinitionName name) _)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate " "
     in mconcat ["(", name, " ", appliedFieldTypes, ")"]
outputDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate " "
     in mconcat ["(", haskellifyModuleName moduleName, ".", name, " ", appliedFieldTypes, ")"]
outputDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate " "
        maybeAppliedOutput = if null appliedTypes then "" else mconcat [" ", appliedFieldTypes]
     in mconcat ["(", haskellifyModuleName moduleName, ".", name, maybeAppliedOutput, ")"]
outputDefinitionReference (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
  mconcat [haskellifyModuleName moduleName, ".", name]

outputBasicType :: BasicTypeValue -> Text
outputBasicType BasicString = "Text"
outputBasicType U8 = "Int"
outputBasicType U16 = "Int"
outputBasicType U32 = "Int"
outputBasicType U64 = "Helpers.BigInteger"
outputBasicType U128 = "Helpers.BigInteger"
outputBasicType I8 = "Int"
outputBasicType I16 = "Int"
outputBasicType I32 = "Int"
outputBasicType I64 = "Helpers.BigInteger"
outputBasicType I128 = "Helpers.BigInteger"
outputBasicType F32 = "Float"
outputBasicType F64 = "Double"
outputBasicType Boolean = "Bool"

fieldTypeName :: FieldType -> Text
fieldTypeName (LiteralType _) = error "Just don't use literals in untagged unions"
fieldTypeName (RecursiveReferenceType _) =
  error "Just don't use recursive references in untagged unions"
fieldTypeName (BasicType BasicString) = "String"
fieldTypeName (BasicType F32) = "F32"
fieldTypeName (BasicType F64) = "F64"
fieldTypeName (BasicType U8) = "U8"
fieldTypeName (BasicType U16) = "U16"
fieldTypeName (BasicType U32) = "U32"
fieldTypeName (BasicType U64) = "U64"
fieldTypeName (BasicType U128) = "U128"
fieldTypeName (BasicType I8) = "I8"
fieldTypeName (BasicType I16) = "I16"
fieldTypeName (BasicType I32) = "I32"
fieldTypeName (BasicType I64) = "I64"
fieldTypeName (BasicType I128) = "I128"
fieldTypeName (BasicType Boolean) = "Boolean"
fieldTypeName (TypeVariableReferenceType (TypeVariable t)) = t
fieldTypeName (ComplexType (ArrayType _ arrayFieldType)) =
  "ArrayOf" <> fieldTypeName arrayFieldType
fieldTypeName (ComplexType (SliceType sliceFieldType)) =
  "SliceOf" <> fieldTypeName sliceFieldType
fieldTypeName (ComplexType (PointerType pointerFieldType)) = fieldTypeName pointerFieldType
fieldTypeName (ComplexType (OptionalType optionalFieldType)) =
  "OptionalOf" <> fieldTypeName optionalFieldType
fieldTypeName
  ( DefinitionReferenceType
      (DefinitionReference (TypeDefinition (DefinitionName definitionName) _))
    ) =
    definitionName
fieldTypeName
  ( DefinitionReferenceType
      (ImportedDefinitionReference _ (TypeDefinition (DefinitionName definitionName) _))
    ) =
    definitionName
fieldTypeName
  ( DefinitionReferenceType
      ( AppliedGenericReference
          fieldTypes
          (TypeDefinition (DefinitionName definitionName) _)
        )
    ) =
    mconcat [definitionName, "Of", fieldTypes & fmap fieldTypeName & mconcat]
fieldTypeName
  ( DefinitionReferenceType
      ( AppliedImportedGenericReference
          _moduleName
          (AppliedTypes fieldTypes)
          (TypeDefinition (DefinitionName definitionName) _)
        )
    ) =
    mconcat [definitionName, "Of", fieldTypes & fmap fieldTypeName & mconcat]
fieldTypeName
  ( DefinitionReferenceType
      ( GenericDeclarationReference
          (ModuleName _moduleName)
          (DefinitionName definitionName)
          (AppliedTypes fieldTypes)
        )
    ) =
    mconcat [definitionName, "Of", fieldTypes & fmap fieldTypeName & mconcat]
fieldTypeName
  (DefinitionReferenceType (DeclarationReference _moduleName (DefinitionName definitionName))) =
    definitionName

joinTypeVariables :: [TypeVariable] -> Text
joinTypeVariables typeVariables =
  typeVariables
    & fmap (\(TypeVariable t) -> haskellifyTypeVariable t)
    & Text.intercalate " "
    & (" " <>)
