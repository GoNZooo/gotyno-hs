module CodeGeneration.Kotlin (outputModule) where

import CodeGeneration.Utilities (typeVariablesFrom, upperCaseFirstCharacter)
import RIO
import qualified RIO.Text as Text
import Types

outputModule :: Module -> Text
outputModule Module {name = ModuleName _name, definitions, imports, declarationNames} =
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
        [ modulePrelude,
          "\n\n",
          if Text.null importsOutput then "" else importsOutput <> "\n\n",
          if Text.null declarationImportsOutput then "" else declarationImportsOutput <> "\n\n",
          definitionOutput
        ]

haskellifyModuleName :: Text -> Text
haskellifyModuleName = upperCaseFirstCharacter

modulePrelude :: Text
modulePrelude =
  mconcat
    [ "import com.fasterxml.jackson.annotation.*\n",
      "import com.fasterxml.jackson.module.kotlin.*"
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

-- @TODO: add expansion of structs here to insert the fields of the struct into the constructors'
-- data classes
outputEmbeddedUnion :: DefinitionName -> FieldName -> [EmbeddedConstructor] -> Text
outputEmbeddedUnion unionName typeTag constructors =
  let typeOutput = outputCaseUnion unionName typeTag constructorsAsConstructors []
      constructorsAsConstructors = embeddedConstructorsToConstructors constructors
   in mconcat
        [ outputUnionTypeInfo typeTag,
          "sealed class ",
          unDefinitionName unionName,
          " {\n",
          typeOutput
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
  let valuesOutput =
        values
          & fmap
            ( \(EnumerationValue (EnumerationIdentifier i) literal) ->
                mconcat ["    ", i, " = ", outputLiteralValue literal]
            )
          & Text.intercalate ",\n"
      outputLiteralValue (LiteralString t) = mconcat ["\"", t, "\""]
      outputLiteralValue (LiteralInteger i) = tshow i
      outputLiteralValue (LiteralFloat f) = tshow f
      outputLiteralValue (LiteralBoolean b) = bool "false" "true" b
   in mconcat
        [ mconcat ["enum class ", unDefinitionName name, " {\n"],
          valuesOutput,
          "\n",
          "}"
        ]

outputPlainStruct :: DefinitionName -> [StructField] -> Text
outputPlainStruct name fields =
  let fieldsOutput = fields & fmap outputField & Text.intercalate ",\n    "
      typeOutput = mconcat ["data class ", unDefinitionName name]
   in mconcat [typeOutput, "(\n    ", fieldsOutput, "\n)"]

outputGenericStruct :: DefinitionName -> [TypeVariable] -> [StructField] -> Text
outputGenericStruct name typeVariables fields =
  let fullName = unDefinitionName name <> joinTypeVariables typeVariables
      deriveLensAndJSONOutput = mconcat ["deriveLensAndJSON ''", unDefinitionName name]
      fieldsOutput = fields & fmap outputField & Text.intercalate ",\n    "
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
  let caseUnionOutput = outputCaseUnion name typeTag (constructorsFrom unionType) typeVariables
      constructorsFrom (PlainUnion constructors) = constructors
      constructorsFrom (GenericUnion _typeVariables constructors) = constructors
      maybeTypeVariables =
        if null typeVariables
          then ""
          else mconcat ["<", Text.intercalate ", " (unTypeVariable <$> typeVariables), ">"]
      typeOutput = mconcat ["sealed class ", unDefinitionName name, maybeTypeVariables]
      typeVariables = case unionType of
        PlainUnion _constructors -> []
        GenericUnion ts _constructors -> ts
   in mconcat
        [ outputUnionTypeInfo typeTag,
          "\n",
          typeOutput,
          " {\n",
          caseUnionOutput,
          "\n}"
        ]

outputCaseUnion :: DefinitionName -> FieldName -> [Constructor] -> [TypeVariable] -> Text
outputCaseUnion unionName _typeTag constructors typeVariables =
  constructors & fmap outputDataClass & Text.intercalate "\n\n"
  where
    outputDataClass (Constructor name maybeFieldType) =
      let maybeTypeVariables =
            if null typeVariablesForConstructor
              then ""
              else mconcat ["<", joinTypeVariables typeVariablesForConstructor, ">"]
          typeVariablesForConstructor = maybe [] (typeVariablesFrom >>> concat) maybeFieldType
          dataFieldOutput = mconcat ["val data: ", maybe "Unit = Unit" outputFieldType maybeFieldType]
          constructorInfo =
            mconcat $
              ["    @JsonTypeName(\"", unConstructorName name, "\")\n"]
                <> maybe
                  ["    @JsonInclude(JsonInclude.Include.NON_DEFAULT)\n"]
                  (const [])
                  maybeFieldType
       in mconcat
            [ constructorInfo,
              "    data class ",
              unConstructorName name,
              maybeTypeVariables,
              "(",
              dataFieldOutput,
              ") : ",
              unDefinitionName unionName,
              maybeUnionTypeVariableOutput,
              "()"
            ]
    maybeUnionTypeVariableOutput =
      if null typeVariables then "" else mconcat ["<", joinTypeVariables typeVariables, ">"]

outputUnionTypeInfo :: FieldName -> Text
outputUnionTypeInfo typeTag =
  mconcat
    [ "@JsonTypeInfo(\n",
      "    use = JsonTypeInfo.Id.NAME,\n",
      "    include = JsonTypeInfo.As.PROPERTY,\n",
      "    property = \"",
      unFieldName typeTag,
      "\"\n",
      ")"
    ]

outputField :: StructField -> Text
outputField (StructField fieldName fieldType) =
  mconcat ["val ", unFieldName fieldName, ": ", outputFieldType fieldType]

outputFieldType :: FieldType -> Text
outputFieldType (LiteralType (LiteralString text)) =
  mconcat [outputBasicType BasicString, " = \"", text, "\""]
outputFieldType (LiteralType (LiteralInteger _x)) = outputBasicType I32
outputFieldType (LiteralType (LiteralFloat _f)) = outputBasicType F32
outputFieldType (LiteralType (LiteralBoolean _b)) = outputBasicType Boolean
outputFieldType (BasicType basicType) = outputBasicType basicType
outputFieldType (ComplexType (OptionalType fieldType)) =
  mconcat [outputFieldType fieldType, "?"]
outputFieldType (ComplexType (ArrayType _size fieldType)) =
  mconcat ["Array<", outputFieldType fieldType, ">"]
outputFieldType (ComplexType (SliceType fieldType)) =
  mconcat ["Array<", outputFieldType fieldType, ">"]
outputFieldType (ComplexType (PointerType fieldType)) = outputFieldType fieldType
outputFieldType (RecursiveReferenceType (DefinitionName name)) = name
outputFieldType (DefinitionReferenceType definitionReference) =
  outputDefinitionReference definitionReference
outputFieldType (TypeVariableReferenceType t) = unTypeVariable t

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
     in mconcat [name, "<", appliedFieldTypes, ">"]
outputDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate " "
     in mconcat [moduleName, ".", name, "<", appliedFieldTypes, ">"]
outputDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate " "
        maybeAppliedOutput = if null appliedTypes then "" else mconcat [" ", appliedFieldTypes]
     in mconcat ["(", moduleName, ".", name, maybeAppliedOutput, ")"]
outputDefinitionReference (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
  mconcat [moduleName, ".", name]

outputBasicType :: BasicTypeValue -> Text
outputBasicType BasicString = "String"
outputBasicType U8 = "UByte"
outputBasicType U16 = "UShort"
outputBasicType U32 = "UInt"
outputBasicType U64 = "gotynoHelpers.U64"
outputBasicType U128 = "gotynoHelpers.U128"
outputBasicType I8 = "Byte"
outputBasicType I16 = "Short"
outputBasicType I32 = "Int"
outputBasicType I64 = "gotynoHelpers.I64"
outputBasicType I128 = "gotynoHelpers.U128"
outputBasicType F32 = "Float"
outputBasicType F64 = "Double"
outputBasicType Boolean = "Boolean"

fieldTypeName :: FieldType -> Text
fieldTypeName (LiteralType (LiteralString s)) = mconcat ["\"", s, "\""]
fieldTypeName (LiteralType (LiteralBoolean b)) = mconcat ["Bool", tshow b]
fieldTypeName (LiteralType (LiteralFloat f)) =
  mconcat
    [ "Float",
      f & tshow
        & Text.map
          ( \case
              '.' -> '_'
              c -> c
          )
    ]
fieldTypeName (LiteralType (LiteralInteger i)) = mconcat ["Integer", tshow i]
fieldTypeName (RecursiveReferenceType (DefinitionName name)) = name
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
  typeVariables & fmap unTypeVariable & Text.intercalate ", "
