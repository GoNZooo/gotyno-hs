module CodeGeneration.FSharp where

import RIO
import qualified RIO.Char as Char
import qualified RIO.List.Partial as PartialList
import qualified RIO.Text as Text
import Types

outputModule :: Module -> Text
outputModule Module {name = ModuleName name, definitions, imports} =
  let definitionOutput = definitions & fmap outputDefinition & Text.intercalate "\n\n"
      _importsOutput = imports & fmap outputImport & mconcat
      outputImport (Import Module {name = ModuleName _name}) =
        mconcat ["import * as ", name, " from \"./", name, "\";\n\n"]
   in mconcat [modulePrelude (fsharpifyModuleName name), definitionOutput]

fsharpifyModuleName :: Text -> Text
fsharpifyModuleName = upperCaseFirstCharacter

modulePrelude :: Text -> Text
modulePrelude name =
  mconcat
    [ mconcat ["module ", name, "\n\n"],
      "open Thoth.Json.Net\n\n"
    ]

outputDefinition :: TypeDefinition -> Text
outputDefinition (TypeDefinition (DefinitionName name) (Struct (PlainStruct fields))) =
  outputPlainStruct name fields
outputDefinition (TypeDefinition (DefinitionName name) (Struct (GenericStruct typeVariables fields))) =
  outputGenericStruct name typeVariables fields
outputDefinition (TypeDefinition (DefinitionName name) (Union typeTag unionType)) =
  outputUnion name typeTag unionType
outputDefinition (TypeDefinition (DefinitionName name) (Enumeration enumerationValues)) =
  outputEnumeration name enumerationValues
outputDefinition (TypeDefinition (DefinitionName name) (UntaggedUnion unionCases)) =
  outputUntaggedUnion name unionCases
outputDefinition (TypeDefinition (DefinitionName name) (EmbeddedUnion typeTag constructors)) =
  outputEmbeddedUnion name typeTag constructors

outputEmbeddedUnion :: Text -> FieldName -> [EmbeddedConstructor] -> Text
outputEmbeddedUnion unionName (FieldName tag) constructors =
  let typeOutput =
        outputCaseUnion
          unionName
          constructorsAsConstructors
          []
      constructorsAsConstructors = embeddedConstructorsToConstructors constructors
      constructorDecodersOutput = outputEmbeddedConstructorDecoders unionName constructors
      tagDecoderPairsOutput =
        constructors
          & fmap
            ( \(EmbeddedConstructor (ConstructorName name) _reference) ->
                mconcat ["                \"", name, "\", ", unionName, ".", name, "Decoder\n"]
            )
          & mconcat
      decoderOutput =
        mconcat
          [ mconcat ["    static member Decoder: Decoder<", unionName, "> =\n"],
            "        GotynoCoders.decodeWithTypeTag\n",
            mconcat ["            \"", tag, "\"\n"],
            "            [|\n",
            tagDecoderPairsOutput,
            "            |]"
          ]
      constructorCasesOutput =
        constructors
          & fmap
            ( \(EmbeddedConstructor (ConstructorName name) reference) ->
                let fields = structFieldsFromReference reference
                    fieldsEncoderOutput =
                      fields
                        & fmap
                          ( outputEncoderForFieldWithValueName "payload"
                              >>> ("                    " <>)
                              >>> (<> "\n")
                          )
                        & mconcat
                 in mconcat
                      [ mconcat ["        | ", name, " payload ->\n"],
                        "            Encode.object\n",
                        "                [\n",
                        mconcat ["                    \"", tag, "\", Encode.string \"", name, "\"\n"],
                        fieldsEncoderOutput,
                        "                ]"
                      ]
            )
          & Text.intercalate "\n\n"
      encoderOutput =
        mconcat
          [ mconcat ["    static member Encoder =\n"],
            "        function\n",
            constructorCasesOutput
          ]
   in Text.intercalate
        "\n\n"
        [ typeOutput,
          constructorDecodersOutput,
          decoderOutput,
          encoderOutput
        ]

outputEmbeddedConstructorDecoders :: Text -> [EmbeddedConstructor] -> Text
outputEmbeddedConstructorDecoders unionName =
  fmap (outputEmbeddedConstructorDecoder unionName) >>> Text.intercalate "\n\n"

outputEmbeddedConstructorDecoder :: Text -> EmbeddedConstructor -> Text
outputEmbeddedConstructorDecoder unionName (EmbeddedConstructor (ConstructorName name) reference) =
  let structFields = structFieldsFromReference reference
      structFieldDecoders =
        structFields
          & fmap (outputDecoderForField >>> ("                " <>) >>> (<> "\n"))
          & mconcat
   in mconcat
        [ mconcat ["    static member ", name, "Decoder: Decoder<", unionName, "> =\n"],
          "        Decode.object (fun get ->\n",
          mconcat ["            ", name, " {\n"],
          structFieldDecoders,
          "            }\n",
          "        )"
        ]

outputEmbeddedCaseTypeGuards :: FieldName -> Text -> [EmbeddedConstructor] -> Text
outputEmbeddedCaseTypeGuards typeTag unionName =
  fmap (outputEmbeddedCaseTypeGuard typeTag unionName)
    >>> Text.intercalate "\n\n"

outputEmbeddedCaseTypeGuard :: FieldName -> Text -> EmbeddedConstructor -> Text
outputEmbeddedCaseTypeGuard
  (FieldName tag)
  unionName
  (EmbeddedConstructor (ConstructorName name) reference) =
    let fields = structFieldsFromReference reference
        tagName = unionEnumConstructorTag unionName name
        interface = mconcat ["{", tag, ": ", tagName, ", ", fieldTypeGuards, "}"]
        fieldTypeGuards = fields & fmap outputDecoderForField & Text.intercalate ", "
     in mconcat
          [ mconcat ["export function is", name, "(value: unknown): value is ", name, " {\n"],
            mconcat ["    return svt.isInterface<", name, ">(value, ", interface, ");\n"],
            "}"
          ]

outputEmbeddedCaseValidators :: FieldName -> Text -> [EmbeddedConstructor] -> Text
outputEmbeddedCaseValidators typeTag unionName =
  fmap (outputEmbeddedCaseValidator typeTag unionName)
    >>> Text.intercalate "\n\n"

outputEmbeddedCaseValidator :: FieldName -> Text -> EmbeddedConstructor -> Text
outputEmbeddedCaseValidator
  (FieldName tag)
  unionName
  (EmbeddedConstructor (ConstructorName name) reference) =
    let fields = structFieldsFromReference reference
        tagName = unionEnumConstructorTag unionName name
        interface = mconcat ["{", tag, ": ", tagName, ", ", fieldValidators, "}"]
        fieldValidators = fields & fmap outputValidatorForField & Text.intercalate ", "
     in mconcat
          [ mconcat
              [ "export function validate",
                name,
                "(value: unknown): svt.ValidationResult<",
                name,
                "> {\n"
              ],
            mconcat ["    return svt.validate<", name, ">(value, ", interface, ");\n"],
            "}"
          ]

outputEmbeddedConstructorTypes :: Text -> FieldName -> [EmbeddedConstructor] -> Text
outputEmbeddedConstructorTypes unionName fieldName constructors =
  constructors & fmap (outputEmbeddedConstructorType unionName fieldName) & Text.intercalate "\n\n"

outputEmbeddedConstructorType :: Text -> FieldName -> EmbeddedConstructor -> Text
outputEmbeddedConstructorType
  unionName
  (FieldName tag)
  (EmbeddedConstructor (ConstructorName name) fields) =
    let fieldsOutput =
          fields
            & structFieldsFromReference
            & fmap (outputField 8)
            & Text.intercalate ""
        tagFieldOutput = mconcat ["    ", tag, ": ", tagValue, ";"]
        tagValue = unionEnumConstructorTag unionName name
     in mconcat
          [ mconcat ["export type ", name, " = {\n"],
            mconcat [tagFieldOutput, "\n"],
            fieldsOutput,
            "};"
          ]

outputEmbeddedCaseConstructors :: Text -> FieldName -> [EmbeddedConstructor] -> Text
outputEmbeddedCaseConstructors unionName typeTag =
  fmap (outputEmbeddedCaseConstructor unionName typeTag)
    >>> Text.intercalate "\n\n"

outputEmbeddedCaseConstructor :: Text -> FieldName -> EmbeddedConstructor -> Text
outputEmbeddedCaseConstructor
  unionName
  (FieldName tag)
  (EmbeddedConstructor (ConstructorName name) definitionReference) =
    mconcat
      [ mconcat
          [ "export function ",
            name,
            "(data: ",
            outputDefinitionReference definitionReference,
            "): ",
            name,
            " {\n"
          ],
        mconcat ["    return {", tag, ": ", unionEnumConstructorTag unionName name, ", ...data};\n"],
        "}"
      ]

structFieldsFromReference :: DefinitionReference -> [StructField]
structFieldsFromReference
  (DefinitionReference (TypeDefinition _name (Struct (PlainStruct fields)))) = fields
structFieldsFromReference _other = error "struct fields from anything other than plain struct"

embeddedConstructorsToConstructors :: [EmbeddedConstructor] -> [Constructor]
embeddedConstructorsToConstructors = fmap embeddedConstructorToConstructor

embeddedConstructorToConstructor :: EmbeddedConstructor -> Constructor
embeddedConstructorToConstructor (EmbeddedConstructor name reference) =
  Constructor name (Just (DefinitionReferenceType reference))

outputUntaggedUnion :: Text -> [FieldType] -> Text
outputUntaggedUnion unionName cases =
  let typeOutput = mconcat ["type ", unionName, " =\n", unionOutput]
      unionOutput = cases & fmap outputCaseLine & Text.intercalate "\n"
      outputCaseLine fieldType =
        mconcat
          [ "    | ",
            unionName,
            fieldTypeName fieldType,
            " of ",
            outputFieldType fieldType
          ]
      decoderOutput = outputUntaggedUnionDecoder unionName cases
      encoderOutput = outputUntaggedUnionEncoder unionName cases
   in Text.intercalate "\n\n" [typeOutput, decoderOutput, encoderOutput]

outputUntaggedUnionDecoder :: Text -> [FieldType] -> Text
outputUntaggedUnionDecoder name cases =
  let caseDecodersOutput = cases & fmap decoderForCase & Text.intercalate "\n\n"
      decoderForCase fieldType =
        let caseName = name <> fieldTypeName fieldType
         in mconcat
              [ mconcat ["    static member ", caseName, "Decoder: Decoder<", name, "> =\n"],
                mconcat ["        Decode.map ", caseName, " ", decoderForFieldType fieldType]
              ]
      oneOfListOutput =
        cases & fmap oneOfCaseOutput & mconcat
      oneOfCaseOutput caseFieldType =
        mconcat ["                ", name, ".", name, fieldTypeName caseFieldType, "Decoder\n"]
   in mconcat
        [ caseDecodersOutput,
          "\n\n",
          mconcat ["    static member Decoder: Decoder<", name, "> =\n"],
          mconcat ["        Decode.oneOf\n"],
          mconcat ["            [\n"],
          oneOfListOutput,
          mconcat ["            ]"]
        ]

outputUntaggedUnionEncoder :: Text -> [FieldType] -> Text
outputUntaggedUnionEncoder name cases =
  let caseEncodersOutput = cases & fmap encoderForCase & Text.intercalate "\n\n"
      encoderForCase caseFieldType =
        mconcat
          [ mconcat
              ["        | ", name, fieldTypeName caseFieldType, " payload ->\n"],
            mconcat
              ["            ", encoderForFieldType ("", "") caseFieldType, " payload"]
          ]
   in mconcat
        [ "    static member Encoder =\n",
          "        function\n",
          caseEncodersOutput
        ]

outputEnumeration :: Text -> [EnumerationValue] -> Text
outputEnumeration name values =
  let typeOutput = outputEnumerationType name values
      decoderOutput = outputEnumerationDecoder name values
      encoderOutput = outputEnumerationEncoder values
   in mconcat [typeOutput, "\n\n", decoderOutput, "\n\n", encoderOutput]

outputEnumerationType :: Text -> [EnumerationValue] -> Text
outputEnumerationType name values =
  let valuesOutput =
        values
          & fmap
            ( \(EnumerationValue (EnumerationIdentifier i) _literal) ->
                mconcat ["    | ", fsharpifyConstructorName i]
            )
          & Text.intercalate "\n"
   in mconcat [mconcat ["type ", name, " =\n"], valuesOutput]

fsharpifyConstructorName :: Text -> Text
fsharpifyConstructorName = upperCaseFirstCharacter

outputEnumerationDecoder :: Text -> [EnumerationValue] -> Text
outputEnumerationDecoder unionName values =
  let valuesOutput =
        values
          & fmap
            ( \(EnumerationValue (EnumerationIdentifier i) value) ->
                mconcat [outputLiteral value, ", ", fsharpifyConstructorName i]
            )
          & Text.intercalate "; "
      outputLiteral (LiteralString s) = "\"" <> s <> "\""
      outputLiteral (LiteralBoolean b) = bool "false" "true" b
      outputLiteral (LiteralInteger i) = tshow i
      outputLiteral (LiteralFloat f) = tshow f
      valuesDecoder =
        values
          & PartialList.head
          & ( \case
                (EnumerationValue _identifier (LiteralString _s)) ->
                  decoderForBasicType BasicString
                (EnumerationValue _identifier (LiteralBoolean _s)) ->
                  decoderForBasicType Boolean
                (EnumerationValue _identifier (LiteralInteger _s)) ->
                  decoderForBasicType I32
                (EnumerationValue _identifier (LiteralFloat _s)) ->
                  decoderForBasicType F32
            )
   in mconcat
        [ mconcat ["    static member Decoder: Decoder<", unionName, "> =\n"],
          mconcat ["        GotynoCoders.decodeOneOf ", valuesDecoder, " [|", valuesOutput, "|]"]
        ]

outputEnumerationEncoder :: [EnumerationValue] -> Text
outputEnumerationEncoder values =
  let caseOutput =
        values
          & fmap caseEncoder
          & Text.intercalate "\n"
      caseEncoder (EnumerationValue (EnumerationIdentifier i) (LiteralString s)) =
        mconcat ["        | ", fsharpifyConstructorName i, " -> Encode.string \"", s, "\""]
      caseEncoder (EnumerationValue (EnumerationIdentifier i) (LiteralBoolean b)) =
        let value = bool "false" "true" b
         in mconcat ["        | ", fsharpifyConstructorName i, " -> Encode.boolean ", value]
      caseEncoder (EnumerationValue (EnumerationIdentifier i) (LiteralInteger integer)) =
        mconcat ["        | ", fsharpifyConstructorName i, " -> Encode.int32 ", tshow integer]
      caseEncoder (EnumerationValue (EnumerationIdentifier i) (LiteralFloat f)) =
        mconcat ["        | ", fsharpifyConstructorName i, " -> Encode.float32 ", tshow f]
   in mconcat
        [ mconcat ["    static member Encoder =\n"],
          mconcat ["        function\n"],
          caseOutput
        ]

outputPlainStruct :: Text -> [StructField] -> Text
outputPlainStruct name fields =
  let fieldsOutput = fields & fmap (outputField 8) & mconcat
      decoderOutput = outputStructDecoder name fields []
      encoderOutput = outputStructEncoder fields []
   in mconcat
        [ mconcat ["type ", name, " =\n"],
          "    {\n",
          fieldsOutput,
          "    }\n\n",
          decoderOutput,
          "\n\n",
          encoderOutput
        ]

outputGenericStruct :: Text -> [TypeVariable] -> [StructField] -> Text
outputGenericStruct name typeVariables fields =
  let fullName = name <> joinTypeVariables typeVariables
      typeOutput =
        mconcat
          [ mconcat ["type ", fullName, " =\n"],
            "    {\n",
            fieldsOutput,
            "    }"
          ]
      fieldsOutput = fields & fmap (outputField 8) & mconcat
      decoderOutput = outputStructDecoder name fields typeVariables
      encoderOutput = outputStructEncoder fields typeVariables
   in mconcat [typeOutput, "\n\n", decoderOutput, "\n\n", encoderOutput]

outputValidatorForField :: StructField -> Text
outputValidatorForField (StructField (FieldName fieldName) fieldType) =
  mconcat [fieldName, ": ", outputValidatorForFieldType fieldType]

outputValidatorForFieldType :: FieldType -> Text
outputValidatorForFieldType (LiteralType (LiteralString text)) = mconcat ["\"", text, "\""]
outputValidatorForFieldType (LiteralType (LiteralInteger x)) = tshow x
outputValidatorForFieldType (LiteralType (LiteralFloat f)) = tshow f
outputValidatorForFieldType (LiteralType (LiteralBoolean b)) = bool "false" "true" b
outputValidatorForFieldType (BasicType basicType) = outputValidatorForBasicType basicType
outputValidatorForFieldType (ComplexType complexType) = outputValidatorForComplexType complexType
outputValidatorForFieldType (DefinitionReferenceType definitionReference) =
  outputValidatorForDefinitionReference definitionReference
outputValidatorForFieldType (RecursiveReferenceType (DefinitionName name)) = "validate" <> name
outputValidatorForFieldType (TypeVariableReferenceType (TypeVariable name)) = "validate" <> name

outputValidatorForDefinitionReference :: DefinitionReference -> Text
outputValidatorForDefinitionReference (DefinitionReference (TypeDefinition (DefinitionName name) _typeData)) =
  "validate" <> name
outputValidatorForDefinitionReference
  ( ImportedDefinitionReference
      (ModuleName moduleName)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    mconcat [fsharpifyModuleName moduleName, ".validate", name]
outputValidatorForDefinitionReference
  ( AppliedGenericReference
      appliedTypes
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedValidators = appliedTypes & fmap outputValidatorForFieldType & Text.intercalate ", "
     in mconcat ["validate", name, "(", appliedValidators, ")"]
outputValidatorForDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedValidators = appliedTypes & fmap outputValidatorForFieldType & Text.intercalate ", "
     in mconcat [fsharpifyModuleName moduleName, ".validate", name, "(", appliedValidators, ")"]

outputValidatorForBasicType :: BasicTypeValue -> Text
outputValidatorForBasicType BasicString = "svt.validateString"
outputValidatorForBasicType U8 = "svt.validateNumber"
outputValidatorForBasicType U16 = "svt.validateNumber"
outputValidatorForBasicType U32 = "svt.validateNumber"
outputValidatorForBasicType U64 = "svt.validateNumber"
outputValidatorForBasicType U128 = "svt.validateNumber"
outputValidatorForBasicType I8 = "svt.validateNumber"
outputValidatorForBasicType I16 = "svt.validateNumber"
outputValidatorForBasicType I32 = "svt.validateNumber"
outputValidatorForBasicType I64 = "svt.validateNumber"
outputValidatorForBasicType I128 = "svt.validateNumber"
outputValidatorForBasicType F32 = "svt.validateNumber"
outputValidatorForBasicType F64 = "svt.validateNumber"
outputValidatorForBasicType Boolean = "svt.validateBoolean"

outputValidatorForComplexType :: ComplexTypeValue -> Text
outputValidatorForComplexType (ArrayType _size typeData) =
  mconcat ["svt.validateArray(", outputValidatorForFieldType typeData, ")"]
outputValidatorForComplexType (SliceType typeData) =
  mconcat ["svt.validateArray(", outputValidatorForFieldType typeData, ")"]
outputValidatorForComplexType (PointerType typeData) =
  outputValidatorForFieldType typeData
outputValidatorForComplexType (OptionalType typeData) =
  mconcat ["svt.validateOptional(", outputValidatorForFieldType typeData, ")"]

outputStructDecoder :: Text -> [StructField] -> [TypeVariable] -> Text
outputStructDecoder name fields typeVariables =
  let prelude =
        mconcat ["    static member Decoder", maybeArguments, ": Decoder<", fullName, "> =\n"]
      fullName =
        if null typeVariables then name else mconcat [name, joinTypeVariables typeVariables]
      maybeArguments = typeVariableDecodersAsArguments typeVariables
      interface =
        fields & fmap (outputDecoderForField >>> addIndentation) & Text.intercalate "\n"
      addIndentation = ("                " <>)
   in mconcat
        [ mconcat
            [ prelude,
              "        Decode.object (fun get ->\n",
              "            {\n",
              mconcat [interface, "\n"],
              "            }\n",
              "        )"
            ]
        ]

outputStructEncoder :: [StructField] -> [TypeVariable] -> Text
outputStructEncoder fields typeVariables =
  let prelude =
        mconcat ["    static member Encoder", maybeArguments, " value =\n"]
      maybeArguments = typeVariableEncodersAsArguments typeVariables
      interface =
        fields & fmap (outputEncoderForField >>> addIndentation) & Text.intercalate "\n"
      addIndentation = ("                " <>)
   in mconcat
        [ mconcat
            [ prelude,
              "        Encode.object\n",
              "            [\n",
              mconcat [interface, "\n"],
              "            ]"
            ]
        ]

outputDecoderForField :: StructField -> Text
outputDecoderForField (StructField (FieldName fieldName) (ComplexType (OptionalType fieldType))) =
  mconcat
    [ sanitizeName fieldName,
      " = ",
      "get.Optional.Field",
      " \"",
      fieldName,
      "\" ",
      decoderForFieldType fieldType
    ]
outputDecoderForField (StructField (FieldName fieldName) fieldType) =
  mconcat
    [ sanitizeName fieldName,
      " = ",
      "get.Required.Field",
      " \"",
      fieldName,
      "\" ",
      decoderForFieldType fieldType
    ]

outputEncoderForField :: StructField -> Text
outputEncoderForField = outputEncoderForFieldWithValueName "value"

outputEncoderForFieldWithValueName :: Text -> StructField -> Text
outputEncoderForFieldWithValueName
  _valueName
  (StructField (FieldName fieldName) fieldType@(LiteralType _)) =
    mconcat ["\"", fieldName, "\", ", encoderForFieldType ("", "") fieldType]
outputEncoderForFieldWithValueName valueName (StructField (FieldName fieldName) fieldType) =
  mconcat
    [ "\"",
      fieldName,
      "\", ",
      encoderForFieldType ("", "") fieldType,
      " ",
      valueName,
      ".",
      fieldName
    ]

decoderForFieldType :: FieldType -> Text
decoderForFieldType (LiteralType literalType) = decoderForLiteralType literalType
decoderForFieldType (BasicType basicType) = decoderForBasicType basicType
decoderForFieldType (ComplexType complexType) = decoderForComplexType complexType
decoderForFieldType (DefinitionReferenceType definitionReference) =
  decoderForDefinitionReference definitionReference
decoderForFieldType (TypeVariableReferenceType (TypeVariable name)) = "decode" <> name
decoderForFieldType (RecursiveReferenceType (DefinitionName name)) = name <> ".Decoder"

decoderForBasicType :: BasicTypeValue -> Text
decoderForBasicType BasicString = "Decode.string"
decoderForBasicType U8 = "Decode.uint8"
decoderForBasicType U16 = "Decode.uint16"
decoderForBasicType U32 = "Decode.uint32"
decoderForBasicType U64 = "Decode.uint64"
decoderForBasicType U128 = "Decode.uint128"
decoderForBasicType I8 = "Decode.int8"
decoderForBasicType I16 = "Decode.int16"
decoderForBasicType I32 = "Decode.int32"
decoderForBasicType I64 = "Decode.int64"
decoderForBasicType I128 = "Decode.int128"
decoderForBasicType F32 = "Decode.float32"
decoderForBasicType F64 = "Decode.float64"
decoderForBasicType Boolean = "Decode.boolean"

decoderForLiteralType :: LiteralTypeValue -> Text
decoderForLiteralType (LiteralString s) = "(GotynoCoders.decodeLiteralString \"" <> s <> "\")"
decoderForLiteralType (LiteralInteger i) = "(GotynoCoders.decodeLiteralInteger " <> tshow i <> ")"
decoderForLiteralType (LiteralFloat f) = "(GotynoCoders.decodeLiteralFloat " <> tshow f <> ")"
decoderForLiteralType (LiteralBoolean b) =
  "(GotynoCoders.decodeLiteralBoolean " <> bool "false" "true" b <> ")"

decoderForComplexType :: ComplexTypeValue -> Text
decoderForComplexType (PointerType fieldType) = decoderForFieldType fieldType
decoderForComplexType (ArrayType _size fieldType) =
  mconcat ["(Decode.list ", decoderForFieldType fieldType, ")"]
decoderForComplexType (SliceType fieldType) =
  mconcat ["(Decode.list ", decoderForFieldType fieldType, ")"]
decoderForComplexType (OptionalType fieldType) =
  mconcat ["(Decode.option ", decoderForFieldType fieldType, ")"]

decoderForDefinitionReference :: DefinitionReference -> Text
decoderForDefinitionReference
  ( DefinitionReference
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    name <> ".Decoder"
decoderForDefinitionReference
  ( ImportedDefinitionReference
      (ModuleName moduleName)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    mconcat [fsharpifyModuleName moduleName, ".", name, ".Decoder"]
decoderForDefinitionReference
  ( AppliedGenericReference
      appliedTypes
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedDecoders = appliedTypes & fmap decoderForFieldType & Text.intercalate " "
     in mconcat ["(", name, ".Decoder ", appliedDecoders, ")"]
decoderForDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedDecoders = appliedTypes & fmap decoderForFieldType & Text.intercalate " "
     in mconcat ["(", fsharpifyModuleName moduleName, ".", name, ".Decoder ", appliedDecoders, ")"]

encoderForFieldType :: (Text, Text) -> FieldType -> Text
encoderForFieldType (_l, _r) (LiteralType literalType) = encoderForLiteralType literalType
encoderForFieldType (_l, _r) (BasicType basicType) = encoderForBasicType basicType
encoderForFieldType (_l, _r) (ComplexType complexType@(PointerType _)) =
  encoderForComplexType complexType
encoderForFieldType (l, r) (ComplexType complexType) = l <> encoderForComplexType complexType <> r
encoderForFieldType (_l, _r) (DefinitionReferenceType definitionReference) =
  encoderForDefinitionReference definitionReference
encoderForFieldType (_l, _r) (TypeVariableReferenceType (TypeVariable name)) = "encode" <> name
encoderForFieldType (_l, _r) (RecursiveReferenceType (DefinitionName name)) = name <> ".Encoder"

encoderForBasicType :: BasicTypeValue -> Text
encoderForBasicType BasicString = "Encode.string"
encoderForBasicType U8 = "Encode.uint8"
encoderForBasicType U16 = "Encode.uint16"
encoderForBasicType U32 = "Encode.uint32"
encoderForBasicType U64 = "Encode.uint64"
encoderForBasicType U128 = "Encode.uint128"
encoderForBasicType I8 = "Encode.int8"
encoderForBasicType I16 = "Encode.int16"
encoderForBasicType I32 = "Encode.int32"
encoderForBasicType I64 = "Encode.int64"
encoderForBasicType I128 = "Encode.int128"
encoderForBasicType F32 = "Encode.float32"
encoderForBasicType F64 = "Encode.float64"
encoderForBasicType Boolean = "Encode.boolean"

encoderForLiteralType :: LiteralTypeValue -> Text
encoderForLiteralType (LiteralString s) = "Encode.string \"" <> s <> "\""
encoderForLiteralType (LiteralInteger i) = "Encode.int32 " <> tshow i
encoderForLiteralType (LiteralFloat f) = "Encode.float32 " <> tshow f
encoderForLiteralType (LiteralBoolean b) =
  "Encode.boolean " <> bool "false" "true" b

encoderForComplexType :: ComplexTypeValue -> Text
encoderForComplexType (PointerType fieldType) = encoderForFieldType ("", "") fieldType
encoderForComplexType (ArrayType _size fieldType) =
  mconcat ["GotynoCoders.encodeList ", encoderForFieldType ("(", ")") fieldType]
encoderForComplexType (SliceType fieldType) =
  mconcat ["GotynoCoders.encodeList ", encoderForFieldType ("(", ")") fieldType]
encoderForComplexType (OptionalType fieldType) =
  mconcat ["Encode.option ", encoderForFieldType ("(", ")") fieldType, ""]

encoderForDefinitionReference :: DefinitionReference -> Text
encoderForDefinitionReference
  ( DefinitionReference
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    name <> ".Encoder"
encoderForDefinitionReference
  ( ImportedDefinitionReference
      (ModuleName moduleName)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    mconcat [fsharpifyModuleName moduleName, ".", name, ".Encoder"]
encoderForDefinitionReference
  ( AppliedGenericReference
      appliedTypes
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedEncoders = appliedTypes & fmap (encoderForFieldType ("", "")) & Text.intercalate " "
     in mconcat ["(", name, ".Encoder ", appliedEncoders, ")"]
encoderForDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedEncoders = appliedTypes & fmap (encoderForFieldType ("", "")) & Text.intercalate " "
     in mconcat ["(", fsharpifyModuleName moduleName, ".", name, ".Encoder ", appliedEncoders, ")"]

outputUnion :: Text -> FieldName -> UnionType -> Text
outputUnion name typeTag unionType =
  let caseUnionOutput = outputCaseUnion name (constructorsFrom unionType) typeVariables
      constructorsFrom (PlainUnion constructors) = constructors
      constructorsFrom (GenericUnion _typeVariables constructors) = constructors
      decoderOutput = outputUnionDecoder typeTag name (constructorsFrom unionType) typeVariables
      encoderOutput = outputUnionEncoder typeTag (constructorsFrom unionType) typeVariables
      typeVariables = case unionType of
        PlainUnion _constructors -> []
        GenericUnion ts _constructors -> ts
   in Text.intercalate
        "\n\n"
        [ caseUnionOutput,
          decoderOutput,
          encoderOutput
        ]

outputUnionDecoder :: FieldName -> Text -> [Constructor] -> [TypeVariable] -> Text
outputUnionDecoder (FieldName tag) unionName constructors typeVariables =
  let constructorDecodersOutput =
        constructors
          & fmap (outputConstructorDecoder unionName typeVariables)
          & Text.intercalate "\n\n"
      tagAndDecoderOutput =
        constructors
          & fmap
            ( \(Constructor (ConstructorName name) payload) ->
                let payloadTypeVariables = fromMaybe [] $ foldMap typeVariablesFrom payload
                    maybeDecoderArguments = typeVariableDecodersAsArguments payloadTypeVariables
                 in mconcat
                      [ "                \"",
                        name,
                        "\", ",
                        unionName,
                        ".",
                        name,
                        "Decoder",
                        maybeDecoderArguments,
                        "\n"
                      ]
            )
          & mconcat
      fullName =
        if null typeVariables
          then unionName
          else mconcat [unionName, joinTypeVariables typeVariables]
      maybeArguments = typeVariableDecodersAsArguments typeVariables
   in mconcat
        [ mconcat [constructorDecodersOutput, "\n\n"],
          mconcat ["    static member Decoder", maybeArguments, ": Decoder<", fullName, "> =\n"],
          mconcat ["        GotynoCoders.decodeWithTypeTag\n"],
          mconcat ["            \"", tag, "\"\n"],
          mconcat ["            [|\n"],
          tagAndDecoderOutput,
          mconcat ["            |]"]
        ]

typeVariableDecodersAsArguments :: [TypeVariable] -> Text
typeVariableDecodersAsArguments [] = ""
typeVariableDecodersAsArguments typeVariables =
  " " <> (decodersForTypeVariables typeVariables & Text.intercalate " ")

decodersForTypeVariables :: [TypeVariable] -> [Text]
decodersForTypeVariables = fmap (TypeVariableReferenceType >>> decoderForFieldType)

typeVariableEncodersAsArguments :: [TypeVariable] -> Text
typeVariableEncodersAsArguments [] = ""
typeVariableEncodersAsArguments typeVariables =
  " " <> (encodersForTypeVariables typeVariables & Text.intercalate " ")

encodersForTypeVariables :: [TypeVariable] -> [Text]
encodersForTypeVariables = fmap (TypeVariableReferenceType >>> encoderForFieldType ("", ""))

outputConstructorDecoder :: Text -> [TypeVariable] -> Constructor -> Text
outputConstructorDecoder unionName typeVariables (Constructor (ConstructorName name) maybePayload) =
  let decoder = maybe alwaysSucceedingDecoder decoderWithDataField maybePayload
      alwaysSucceedingDecoder = mconcat ["Decode.succeed ", name]
      payloadTypeVariables = fromMaybe [] $ foldMap typeVariablesFrom maybePayload
      maybeArguments = typeVariableDecodersAsArguments payloadTypeVariables
      fullName =
        if null typeVariables
          then unionName
          else mconcat [unionName, joinTypeVariables typeVariables]
      decoderWithDataField payload =
        mconcat
          [ "Decode.object (fun get -> ",
            name,
            "(get.Required.Field \"data\" ",
            decoderForFieldType payload,
            "))"
          ]
   in mconcat
        [ mconcat
            [ "    static member ",
              name,
              "Decoder",
              maybeArguments,
              ": Decoder<",
              fullName,
              "> =\n"
            ],
          mconcat ["        ", decoder]
        ]

outputUnionEncoder :: FieldName -> [Constructor] -> [TypeVariable] -> Text
outputUnionEncoder typeTag constructors typeVariables =
  let caseEncodingOutput =
        constructors & fmap (outputConstructorEncoder typeTag) & Text.intercalate "\n\n"
      maybeArguments = typeVariableEncodersAsArguments typeVariables
   in mconcat
        [ mconcat ["    static member Encoder", maybeArguments, " =\n"],
          mconcat ["        function\n"],
          caseEncodingOutput
        ]

outputConstructorEncoder :: FieldName -> Constructor -> Text
outputConstructorEncoder (FieldName tag) (Constructor (ConstructorName name) maybePayload) =
  let dataPart = maybe "" encoderWithDataField maybePayload
      typeTagPart = mconcat ["\"", tag, "\", Encode.string \"", name, "\""]
      dataIndentation = "                            "
      encoderWithDataField payload =
        mconcat ["\n", dataIndentation, "\"data\", ", encoderForFieldType ("", "") payload, " payload"]
      interface = mconcat ["[ ", typeTagPart, dataPart, " ]"]
      maybePayloadPart = maybe "" (const " payload") maybePayload
   in mconcat
        [ mconcat ["        | ", name, maybePayloadPart, " ->\n"],
          mconcat ["            Encode.object ", interface]
        ]

newtype FunctionPrefix = FunctionPrefix Text

newtype ReturnType = ReturnType (Text -> Text)

newtype ReturnExpression = ReturnExpression (Text -> FieldName -> [Constructor] -> Text)

outputUnionFunction ::
  FunctionPrefix ->
  ReturnType ->
  ReturnExpression ->
  FieldName ->
  Text ->
  [Constructor] ->
  Text
outputUnionFunction
  (FunctionPrefix prefix)
  (ReturnType returnType)
  (ReturnExpression returnExpression)
  typeTag
  unionName
  constructors =
    mconcat
      [ mconcat
          [ "export function ",
            prefix,
            unionName,
            "(value: unknown): ",
            returnType unionName,
            " {\n"
          ],
        mconcat ["    return ", returnExpression unionName typeTag constructors, ";\n"],
        "}"
      ]

typeVariablePredicateNames :: [TypeVariable] -> Text
typeVariablePredicateNames =
  fmap (\(TypeVariable t) -> "is" <> t) >>> Text.intercalate ", "

typeVariablePredicateParameters :: [TypeVariable] -> Text
typeVariablePredicateParameters =
  fmap (\(TypeVariable t) -> mconcat ["is", t, ": svt.TypePredicate<", t, ">"])
    >>> Text.intercalate ", "

typeVariableValidatorNames :: [TypeVariable] -> Text
typeVariableValidatorNames =
  fmap (\(TypeVariable t) -> "validate" <> t) >>> Text.intercalate ", "

typeVariableValidatorParameters :: [TypeVariable] -> Text
typeVariableValidatorParameters =
  fmap (\(TypeVariable t) -> mconcat ["validate", t, ": svt.Validator<", t, ">"])
    >>> Text.intercalate ", "

outputCaseValidators :: FieldName -> Text -> [Constructor] -> Text
outputCaseValidators typeTag unionName =
  fmap (outputCaseValidator typeTag unionName) >>> Text.intercalate "\n\n"

outputCaseValidator :: FieldName -> Text -> Constructor -> Text
outputCaseValidator
  (FieldName tag)
  unionName
  (Constructor (ConstructorName name) maybePayload) =
    let tagValue = unionEnumConstructorTag unionName name
        typeVariables = fromMaybe [] $ foldMap typeVariablesFrom maybePayload
        interface =
          mconcat $
            ["{", tag, ": ", tagValue]
              <> maybe ["}"] (\p -> [", data: ", outputValidatorForFieldType p, "}"]) maybePayload
     in if null typeVariables
          then
            mconcat
              [ mconcat
                  [ "export function validate",
                    name,
                    "(value: unknown): svt.ValidationResult<",
                    name,
                    "> {\n"
                  ],
                mconcat ["    return svt.validate<", name, ">(value, ", interface, ");\n"],
                "}"
              ]
          else
            let fullName = name <> joinTypeVariables typeVariables
                returnedFunctionName = "validate" <> Text.filter (\c -> c /= '<' && c /= '>') fullName
             in mconcat
                  [ mconcat
                      [ "export function validate",
                        fullName,
                        "(",
                        typeVariableValidatorParameters typeVariables,
                        "): svt.Validator<",
                        fullName,
                        "> {\n"
                      ],
                    mconcat
                      [ "    return function ",
                        returnedFunctionName,
                        "(value: unknown): svt.ValidationResult<",
                        fullName,
                        "> {\n"
                      ],
                    mconcat
                      [ "        return svt.validate<",
                        fullName,
                        ">(value, ",
                        interface,
                        ");\n"
                      ],
                    "    };\n",
                    "}"
                  ]

outputCaseUnion :: Text -> [Constructor] -> [TypeVariable] -> Text
outputCaseUnion name constructors typeVariables =
  let cases =
        constructors
          & fmap
            ( \(Constructor (ConstructorName constructorName) maybePayload) ->
                let payload = maybe "" (outputFieldType >>> (" of " <>)) maybePayload
                 in mconcat ["    | ", constructorName, payload]
            )
          & Text.intercalate "\n"
      maybeTypeVariables = if null typeVariables then "" else joinTypeVariables typeVariables
   in mconcat
        [ mconcat ["type ", name, maybeTypeVariables, " =\n"],
          cases
        ]

typeVariablesFrom :: FieldType -> Maybe [TypeVariable]
typeVariablesFrom (TypeVariableReferenceType typeVariable) = pure [typeVariable]
typeVariablesFrom (ComplexType (ArrayType _size fieldType)) = typeVariablesFrom fieldType
typeVariablesFrom (ComplexType (SliceType fieldType)) = typeVariablesFrom fieldType
typeVariablesFrom (ComplexType (PointerType fieldType)) = typeVariablesFrom fieldType
typeVariablesFrom (ComplexType (OptionalType fieldType)) = typeVariablesFrom fieldType
typeVariablesFrom (RecursiveReferenceType _name) = Nothing
typeVariablesFrom (LiteralType _) = Nothing
typeVariablesFrom (BasicType _) = Nothing
typeVariablesFrom (DefinitionReferenceType definitionReference) =
  typeVariablesFromReference definitionReference

typeVariablesFromReference :: DefinitionReference -> Maybe [TypeVariable]
typeVariablesFromReference (DefinitionReference definition) = typeVariablesFromDefinition definition
typeVariablesFromReference (ImportedDefinitionReference _moduleName definition) =
  typeVariablesFromDefinition definition
typeVariablesFromReference (AppliedGenericReference fieldTypes _definition) =
  let typeVariables = fieldTypes & fmap typeVariablesFrom & catMaybes & join
   in if null typeVariables then Nothing else Just typeVariables
typeVariablesFromReference
  ( AppliedImportedGenericReference
      _moduleName
      (AppliedTypes fieldTypes)
      _definition
    ) =
    let typeVariables = fieldTypes & fmap typeVariablesFrom & catMaybes & join
     in if null typeVariables then Nothing else Just typeVariables

typeVariablesFromDefinition :: TypeDefinition -> Maybe [TypeVariable]
typeVariablesFromDefinition (TypeDefinition _name (Struct (PlainStruct _))) = Nothing
typeVariablesFromDefinition (TypeDefinition _name (Union _tagType (PlainUnion _))) = Nothing
typeVariablesFromDefinition (TypeDefinition _name (UntaggedUnion _)) = Nothing
typeVariablesFromDefinition (TypeDefinition _name (Enumeration _)) = Nothing
typeVariablesFromDefinition (TypeDefinition _name (EmbeddedUnion _tagType _constructors)) = Nothing
typeVariablesFromDefinition (TypeDefinition _name (Struct (GenericStruct typeVariables _))) =
  pure typeVariables
typeVariablesFromDefinition (TypeDefinition _name (Union _tagType (GenericUnion typeVariables _))) =
  pure typeVariables

outputUnionTagEnumeration :: Text -> [Constructor] -> Text
outputUnionTagEnumeration name constructors =
  let constructorCasesOutput =
        constructors
          & fmap
            ( \(Constructor (ConstructorName constructorName) _payload) ->
                mconcat ["    ", constructorName, " = \"", constructorName, "\",\n"]
            )
          & mconcat
   in mconcat
        [ mconcat ["export enum ", name, "Tag {\n"],
          constructorCasesOutput,
          "}"
        ]

outputCaseTypes :: Text -> FieldName -> [Constructor] -> Text
outputCaseTypes unionName typeTag constructors =
  constructors
    & fmap (outputCaseType unionName typeTag)
    & Text.intercalate "\n\n"

outputCaseType :: Text -> FieldName -> Constructor -> Text
outputCaseType
  unionName
  (FieldName tag)
  (Constructor (ConstructorName name) maybePayload) =
    let payloadLine = maybe "" (\p -> "    data: " <> outputFieldType p <> ";\n") maybePayload
        maybeTypeVariables = maybe "" (typeVariablesFrom >>> maybeJoinTypeVariables) maybePayload
     in mconcat
          [ mconcat ["export type ", name, maybeTypeVariables, " = {\n"],
            mconcat ["    ", tag, ": ", unionEnumConstructorTag unionName name, ";\n"],
            payloadLine,
            "};"
          ]

outputCaseConstructors :: Text -> FieldName -> [Constructor] -> Text
outputCaseConstructors unionName typeTag constructors =
  constructors
    & fmap (outputCaseConstructor unionName typeTag)
    & Text.intercalate "\n\n"

outputCaseConstructor :: Text -> FieldName -> Constructor -> Text
outputCaseConstructor
  unionName
  (FieldName tag)
  (Constructor (ConstructorName name) maybePayload) =
    let argumentFieldAndType = maybe "" (\p -> "data: " <> outputFieldType p) maybePayload
        maybeTypeVariables = maybe "" joinTypeVariables (maybePayload >>= typeVariablesFrom)
     in mconcat
          [ mconcat
              [ "export function ",
                name,
                maybeTypeVariables,
                "(",
                argumentFieldAndType,
                "): ",
                name,
                maybeTypeVariables,
                " {\n"
              ],
            mconcat
              ( [ "    return {",
                  tag,
                  ": ",
                  unionEnumConstructorTag unionName name
                ]
                  <> maybe [] (const [", data"]) maybePayload
              )
              <> "};\n",
            "}"
          ]

unionEnumConstructorTag :: Text -> Text -> Text
unionEnumConstructorTag unionName constructorName = mconcat [unionName, "Tag.", constructorName]

outputField :: Int -> StructField -> Text
outputField indentation (StructField (FieldName name) fieldType) =
  let indent = Text.pack $ replicate indentation ' '
   in indent <> mconcat [sanitizeName name, ": ", outputFieldType fieldType, "\n"]

sanitizeName :: Text -> Text
sanitizeName "type" = "``type``"
sanitizeName name = name

outputFieldType :: FieldType -> Text
outputFieldType (LiteralType (LiteralString _text)) = outputBasicType BasicString
outputFieldType (LiteralType (LiteralInteger _x)) = outputBasicType I32
outputFieldType (LiteralType (LiteralFloat _f)) = outputBasicType F32
outputFieldType (LiteralType (LiteralBoolean _b)) = outputBasicType Boolean
outputFieldType (BasicType basicType) = outputBasicType basicType
outputFieldType (ComplexType (OptionalType fieldType)) =
  mconcat ["option<", outputFieldType fieldType, ">"]
outputFieldType (ComplexType (ArrayType _size fieldType)) =
  mconcat ["list<", outputFieldType fieldType, ">"]
outputFieldType (ComplexType (SliceType fieldType)) =
  mconcat ["list<", outputFieldType fieldType, ">"]
outputFieldType (ComplexType (PointerType fieldType)) = outputFieldType fieldType
outputFieldType (RecursiveReferenceType (DefinitionName name)) = name
outputFieldType (DefinitionReferenceType definitionReference) =
  outputDefinitionReference definitionReference
outputFieldType (TypeVariableReferenceType (TypeVariable t)) = fsharpifyTypeVariable t

fsharpifyTypeVariable :: Text -> Text
fsharpifyTypeVariable t = t & Text.toLower & ("'" <>)

outputDefinitionReference :: DefinitionReference -> Text
outputDefinitionReference (DefinitionReference (TypeDefinition (DefinitionName name) _)) = name
outputDefinitionReference
  ( ImportedDefinitionReference
      (ModuleName moduleName)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    mconcat [fsharpifyModuleName moduleName, ".", name]
outputDefinitionReference
  ( AppliedGenericReference
      appliedTypes
      (TypeDefinition (DefinitionName name) _)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate ", "
     in mconcat [name, "<", appliedFieldTypes, ">"]
outputDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate ", "
     in mconcat [fsharpifyModuleName moduleName, ".", name, "<", appliedFieldTypes, ">"]

outputBasicType :: BasicTypeValue -> Text
outputBasicType BasicString = "string"
outputBasicType U8 = "uint8"
outputBasicType U16 = "uint16"
outputBasicType U32 = "uint32"
outputBasicType U64 = "uint64"
outputBasicType U128 = "uint128"
outputBasicType I8 = "int8"
outputBasicType I16 = "int16"
outputBasicType I32 = "int32"
outputBasicType I64 = "int64"
outputBasicType I128 = "int128"
outputBasicType F32 = "float32"
outputBasicType F64 = "float64"
outputBasicType Boolean = "boolean"

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

maybeJoinTypeVariables :: Maybe [TypeVariable] -> Text
maybeJoinTypeVariables = maybe "" joinTypeVariables

joinTypeVariables :: [TypeVariable] -> Text
joinTypeVariables typeVariables =
  typeVariables
    & fmap (\(TypeVariable t) -> fsharpifyTypeVariable t)
    & Text.intercalate ", "
    & (\o -> "<" <> o <> ">")

upperCaseFirstCharacter :: Text -> Text
upperCaseFirstCharacter t =
  case Text.uncons t of
    Just (c, rest) -> Text.cons (Char.toUpper c) rest
    Nothing -> t
