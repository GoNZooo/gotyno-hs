module CodeGeneration.Kotlin (outputModule) where

import CodeGeneration.Utilities (structFieldsFromReference, upperCaseFirstCharacter)
import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text
import Types

outputModule :: Module -> Text
outputModule Module {name = ModuleName name, definitions, imports, declarationNames} =
  let definitionOutput = definitions & mapMaybe outputDefinition & Text.intercalate "\n\n"
      importsOutput = imports & fmap outputImport & Text.intercalate "\n"
      outputImport (Import Module {name = ModuleName importName}) =
        mconcat ["import org.gotynoOutput.", uppercaseModuleName importName]
      declarationImportsOutput =
        declarationNames
          & fmap
            ( \(ModuleName declarationModuleName) ->
                mconcat
                  [ "import org.gotynoDeclarations.",
                    uppercaseModuleName declarationModuleName
                  ]
            )
          & Text.intercalate "\n"
      moduleClassOutput = mconcat ["class ", uppercaseModuleName name, " {\n"]
   in mconcat
        [ "package org.gotynoOutput\n\n",
          modulePrelude,
          "\n\n",
          if Text.null importsOutput then "" else importsOutput <> "\n\n",
          if Text.null declarationImportsOutput then "" else declarationImportsOutput <> "\n\n",
          moduleClassOutput,
          definitionOutput,
          "\n",
          "}"
        ]

modulePrelude :: Text
modulePrelude =
  mconcat
    [ "import com.fasterxml.jackson.annotation.*\n",
      "import com.fasterxml.jackson.module.kotlin.*\n",
      "import java.math.BigInteger"
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
outputEmbeddedUnion unionName typeTag constructors =
  mconcat
    [ outputUnionTypeInfo typeTag,
      "\n",
      "sealed class ",
      unDefinitionName unionName,
      " {\n",
      outputEmbeddedCaseUnion unionName typeTag constructors [],
      "\n",
      "}"
    ]

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
                mconcat ["    ", i, "(", outputLiteralValue literal, ")"]
            )
          & Text.intercalate ",\n"
      outputLiteralValue (LiteralString t) = mconcat ["\"", t, "\""]
      outputLiteralValue (LiteralInteger i) = tshow i
      outputLiteralValue (LiteralFloat f) = tshow f
      outputLiteralValue (LiteralBoolean b) = bool "false" "true" b
   in mconcat
        [ mconcat ["enum class ", unDefinitionName name, "(val data: Any) {\n"],
          valuesOutput,
          "\n",
          "}"
        ]

outputPlainStruct :: DefinitionName -> [StructField] -> Text
outputPlainStruct name fields =
  let fieldsOutput = fields & List.sortBy literalCompare & fmap outputField & Text.intercalate ",\n    "
      literalCompare (StructField _aName (LiteralType _al)) (StructField _bName (LiteralType _bl)) =
        EQ
      literalCompare (StructField _aName (LiteralType _al)) _other = GT
      literalCompare _other (StructField _aName (LiteralType _bl)) = LT
      literalCompare _a _b = EQ
      typeOutput = mconcat ["data class ", unDefinitionName name]
   in mconcat [typeOutput, "(\n    ", fieldsOutput, "\n)"]

outputGenericStruct :: DefinitionName -> [TypeVariable] -> [StructField] -> Text
outputGenericStruct name typeVariables fields =
  let fieldsOutput = fields & fmap outputField & Text.intercalate ",\n    "
      typeVariablesOutput = mconcat ["<", joinTypeVariables typeVariables, ">"]
      typeOutput = mconcat ["data class ", unDefinitionName name, typeVariablesOutput]
   in mconcat
        [ typeOutput,
          "(\n    ",
          fieldsOutput,
          "\n",
          ")"
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
      let typeVariablesOutput =
            if null typeVariables then "" else mconcat ["<", joinTypeVariables typeVariables, ">"]
          dataFieldOutput =
            mconcat ["val data: ", maybe "Unit = Unit" outputFieldType maybeFieldType]
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
              typeVariablesOutput,
              "(",
              dataFieldOutput,
              ") : ",
              unDefinitionName unionName,
              maybeUnionTypeVariableOutput,
              "()"
            ]
    maybeUnionTypeVariableOutput =
      if null typeVariables then "" else mconcat ["<", joinTypeVariables typeVariables, ">"]

outputEmbeddedCaseUnion :: DefinitionName -> FieldName -> [EmbeddedConstructor] -> [TypeVariable] -> Text
outputEmbeddedCaseUnion unionName _typeTag constructors typeVariables =
  constructors & fmap outputDataClass & Text.intercalate "\n\n"
  where
    outputDataClass (EmbeddedConstructor name maybeDefinitionReference) =
      let typeVariablesOutput =
            if null typeVariables then "" else mconcat ["<", joinTypeVariables typeVariables, ">"]
          dataFieldOutput = maybe "val data: Unit = Unit" outputFields maybeDefinitionReference
          outputFields =
            structFieldsFromReference >>> fmap outputField >>> Text.intercalate ", "
          constructorInfo =
            mconcat $
              ["    @JsonTypeName(\"", unConstructorName name, "\")\n"]
                <> maybe
                  ["    @JsonInclude(JsonInclude.Include.NON_DEFAULT)\n"]
                  (const [])
                  maybeDefinitionReference
       in mconcat
            [ constructorInfo,
              "    data class ",
              unConstructorName name,
              typeVariablesOutput,
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
    mconcat [uppercaseModuleName moduleName, ".", name]
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
     in mconcat [uppercaseModuleName moduleName, ".", name, "<", appliedFieldTypes, ">"]
outputDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate ", "
        maybeAppliedOutput = if null appliedTypes then "" else mconcat ["<", appliedFieldTypes, ">"]
     in mconcat [uppercaseModuleName moduleName, ".", name, maybeAppliedOutput]
outputDefinitionReference (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
  mconcat [uppercaseModuleName moduleName, ".", name]

outputBasicType :: BasicTypeValue -> Text
outputBasicType BasicString = "String"
outputBasicType U8 = "UByte"
outputBasicType U16 = "UShort"
outputBasicType U32 = "UInt"
outputBasicType U64 = "BigInteger"
outputBasicType U128 = "BigInteger"
outputBasicType I8 = "Byte"
outputBasicType I16 = "Short"
outputBasicType I32 = "Int"
outputBasicType I64 = "BigInteger"
outputBasicType I128 = "BigInteger"
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

uppercaseModuleName :: Text -> Text
uppercaseModuleName = upperCaseFirstCharacter