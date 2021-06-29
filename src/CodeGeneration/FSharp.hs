module CodeGeneration.FSharp where

import RIO
import qualified RIO.Text as Text
import Types

outputModule :: Module -> Text
outputModule Module {name = ModuleName name, definitions, imports} =
  let definitionOutput = definitions & fmap outputDefinition & Text.intercalate "\n\n"
      importsOutput = imports & fmap outputImport & mconcat
      outputImport (Import Module {name = ModuleName name}) =
        mconcat ["import * as ", name, " from \"./", name, "\";\n\n"]
   in mconcat [modulePrelude (fsharpifyModuleName name), definitionOutput]

fsharpifyModuleName :: Text -> Text
fsharpifyModuleName = Text.toTitle

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
outputEmbeddedUnion unionName typeTag constructors =
  let typeOutput =
        outputCaseUnion
          unionName
          constructorsAsConstructors
          []
      constructorsAsConstructors = embeddedConstructorsToConstructors constructors
      tagEnumerationOutput = outputUnionTagEnumeration unionName constructorsAsConstructors
      constructorTypesOutput = outputEmbeddedConstructorTypes unionName typeTag constructors
      caseConstructorsOutput = outputEmbeddedCaseConstructors unionName typeTag constructors
      unionTypeGuardOutput = outputUnionTypeGuard typeTag [] unionName constructorsAsConstructors
      caseTypeGuardOutput = outputEmbeddedCaseTypeGuards typeTag unionName constructors
      unionValidatorOutput = outputUnionValidator [] typeTag unionName constructorsAsConstructors
      caseValidatorOutput = outputEmbeddedCaseValidators typeTag unionName constructors
   in Text.intercalate
        "\n\n"
        [ typeOutput,
          tagEnumerationOutput,
          constructorTypesOutput,
          caseConstructorsOutput,
          unionTypeGuardOutput,
          caseTypeGuardOutput,
          unionValidatorOutput,
          caseValidatorOutput
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
  let typeOutput = mconcat ["export type ", unionName, " = ", unionOutput, ";"]
      unionOutput = cases & fmap outputFieldType & Text.intercalate " | "
      typeGuardOutput = outputUntaggedUnionTypeGuard unionName cases
      validatorOutput = outputUntaggedUnionValidator unionName cases
   in Text.intercalate "\n\n" [typeOutput, typeGuardOutput, validatorOutput]

outputUntaggedUnionTypeGuard :: Text -> [FieldType] -> Text
outputUntaggedUnionTypeGuard name cases =
  let typeGuards = cases & fmap decoderForFieldType & Text.intercalate ", "
   in mconcat
        [ mconcat ["export function is", name, "(value: unknown): value is ", name, " {\n"],
          mconcat ["    return [", typeGuards, "].some((typePredicate) => typePredicate(value));\n"],
          "}"
        ]

outputUntaggedUnionValidator :: Text -> [FieldType] -> Text
outputUntaggedUnionValidator name cases =
  let validators = cases & fmap outputValidatorForFieldType & Text.intercalate ", "
   in mconcat
        [ mconcat
            [ "export function validate",
              name,
              "(value: unknown): svt.ValidationResult<",
              name,
              "> {\n"
            ],
          mconcat ["    return svt.validateOneOf<", name, ">(value, [", validators, "]);\n"],
          "}"
        ]

outputEnumeration :: Text -> [EnumerationValue] -> Text
outputEnumeration name values =
  let typeOutput = outputEnumerationType name values
      typeGuardOutput = outputEnumerationTypeGuard name values
      validatorOutput = outputEnumerationValidator name values
   in mconcat [typeOutput, "\n\n", typeGuardOutput, "\n\n", validatorOutput]

outputEnumerationType :: Text -> [EnumerationValue] -> Text
outputEnumerationType name values =
  let valuesOutput =
        values
          & fmap
            ( \(EnumerationValue (EnumerationIdentifier i) literal) ->
                mconcat ["    ", i, " = ", outputLiteral literal, ",\n"]
            )
          & mconcat
      outputLiteral (LiteralString s) = mconcat ["\"", s, "\""]
      outputLiteral (LiteralInteger i) = tshow i
      outputLiteral (LiteralFloat f) = tshow f
      outputLiteral (LiteralBoolean b) = bool "false" "true" b
   in mconcat
        [ mconcat ["export enum ", name, " {\n"],
          valuesOutput,
          "}"
        ]

outputEnumerationFunction ::
  FunctionPrefix ->
  ReturnType ->
  (Text -> [EnumerationValue] -> Text) ->
  Text ->
  [EnumerationValue] ->
  Text
outputEnumerationFunction
  (FunctionPrefix prefix)
  (ReturnType returnType)
  returnExpression
  name
  values =
    mconcat
      [ mconcat ["export function ", prefix, name, "(value: unknown): ", returnType name, " {\n"],
        mconcat ["    return ", returnExpression name values, ";\n"],
        "}"
      ]

outputEnumerationTypeGuard :: Text -> [EnumerationValue] -> Text
outputEnumerationTypeGuard =
  let returnExpression name values =
        let valuesOutput =
              values
                & fmap (\(EnumerationValue (EnumerationIdentifier i) _value) -> name <> "." <> i)
                & Text.intercalate ", "
         in mconcat ["[", valuesOutput, "].some((v) => v === value)"]
      returnType name = "value is " <> name
   in outputEnumerationFunction (FunctionPrefix "is") (ReturnType returnType) returnExpression

outputEnumerationValidator :: Text -> [EnumerationValue] -> Text
outputEnumerationValidator =
  let returnExpression name values =
        let valuesOutput =
              values
                & fmap (\(EnumerationValue (EnumerationIdentifier i) _value) -> name <> "." <> i)
                & Text.intercalate ", "
         in mconcat ["svt.validateOneOfLiterals<", name, ">(value, ", "[", valuesOutput, "])"]
      returnType name = "svt.ValidationResult<" <> name <> ">"
   in outputEnumerationFunction (FunctionPrefix "validate") (ReturnType returnType) returnExpression

outputPlainStruct :: Text -> [StructField] -> Text
outputPlainStruct name fields =
  let fieldsOutput = fields & fmap (outputField 8) & mconcat
      decoderOutput = outputStructDecoder name fields []
      encoderOutput = outputStructEncoder name fields []
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
          [ mconcat ["export type ", fullName, " = {\n"],
            fieldsOutput,
            "};"
          ]
      fieldsOutput = fields & fmap (outputField 8) & mconcat
      typeGuardOutput = outputStructDecoder name fields typeVariables
      validatorOutput = outputStructValidator name fields typeVariables
   in mconcat [typeOutput, "\n\n", typeGuardOutput, "\n\n", validatorOutput]

outputStructValidator :: Text -> [StructField] -> [TypeVariable] -> Text
outputStructValidator name fields typeVariables =
  let interface =
        "{" <> (fields & fmap outputValidatorForField & Text.intercalate ", ") <> "}"
   in if null typeVariables
        then
          mconcat
            [ mconcat
                [ mconcat
                    [ "export function validate",
                      name,
                      "(value: unknown): svt.ValidationResult<",
                      name,
                      "> {\n"
                    ],
                  mconcat ["    return svt.validate<", name, ">(value, ", interface, ");\n"]
                ],
              "}"
            ]
        else
          let fullName = name <> joinTypeVariables typeVariables
              returnedFunctionName =
                ("validate" <> fullName) & Text.filter ((`elem` ("<>, " :: String)) >>> not)
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
    mconcat [moduleName, ".validate", name]
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
     in mconcat [moduleName, ".validate", name, "(", appliedValidators, ")"]

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
        mconcat ["    static member Decoder: Decoder<", name, "> =\n"]
      interface =
        fields & fmap (outputDecoderForField >>> addIndentation) & Text.intercalate "\n"
      addIndentation = ("                " <>)
   in if null typeVariables
        then
          mconcat
            [ mconcat
                [ prelude,
                  "        Decode.object (fun get ->\n",
                  "            {\n",
                  mconcat [interface, "\n"],
                  "            }\n",
                  "        )"
                ]
            ]
        else
          let fullName = name <> joinTypeVariables typeVariables
              returnedFunctionName =
                ("is" <> fullName) & Text.filter ((`elem` ("<>, " :: String)) >>> not)
           in mconcat
                [ mconcat
                    [ "export function is",
                      fullName,
                      "(",
                      typeVariablePredicateParameters typeVariables,
                      "): svt.TypePredicate<",
                      fullName,
                      "> {\n"
                    ],
                  mconcat
                    [ "    return function ",
                      returnedFunctionName,
                      "(value: unknown): value is ",
                      fullName,
                      " {\n"
                    ],
                  mconcat
                    [ "        return svt.isInterface<",
                      fullName,
                      ">(value, ",
                      interface,
                      ");\n"
                    ],
                  "    };\n",
                  "}"
                ]

outputStructEncoder :: Text -> [StructField] -> [TypeVariable] -> Text
outputStructEncoder name fields typeVariables =
  let prelude =
        mconcat ["    static member Encoder value =\n"]
      interface =
        fields & fmap (outputEncoderForField >>> addIndentation) & Text.intercalate "\n"
      addIndentation = ("                " <>)
   in if null typeVariables
        then
          mconcat
            [ mconcat
                [ prelude,
                  "        Encode.object\n",
                  "            [\n",
                  mconcat [interface, "\n"],
                  "            ]"
                ]
            ]
        else
          let fullName = name <> joinTypeVariables typeVariables
              returnedFunctionName =
                ("is" <> fullName) & Text.filter ((`elem` ("<>, " :: String)) >>> not)
           in mconcat
                [ mconcat
                    [ "export function is",
                      fullName,
                      "(",
                      typeVariablePredicateParameters typeVariables,
                      "): svt.TypePredicate<",
                      fullName,
                      "> {\n"
                    ],
                  mconcat
                    [ "    return function ",
                      returnedFunctionName,
                      "(value: unknown): value is ",
                      fullName,
                      " {\n"
                    ],
                  mconcat
                    [ "        return svt.isInterface<",
                      fullName,
                      ">(value, ",
                      interface,
                      ");\n"
                    ],
                  "    };\n",
                  "}"
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
outputEncoderForField (StructField (FieldName fieldName) fieldType@(LiteralType _)) =
  mconcat ["\"", fieldName, "\", ", encoderForFieldType ("", "") fieldType]
outputEncoderForField (StructField (FieldName fieldName) fieldType) =
  mconcat ["\"", fieldName, "\", ", encoderForFieldType ("", "") fieldType, " value.", fieldName]

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
    let appliedDecoders = appliedTypes & fmap decoderForFieldType & Text.intercalate ", "
     in mconcat [name, ".Decoder ", appliedDecoders]
decoderForDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedDecoders = appliedTypes & fmap decoderForFieldType & Text.intercalate ", "
     in mconcat [fsharpifyModuleName moduleName, ".", name, ".Decoder ", appliedDecoders]

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
    let appliedEncoders = appliedTypes & fmap (encoderForFieldType ("", "")) & Text.intercalate ", "
     in mconcat [name, ".Encoder ", appliedEncoders]
encoderForDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedEncoders = appliedTypes & fmap (encoderForFieldType ("", "")) & Text.intercalate ", "
     in mconcat [fsharpifyModuleName moduleName, ".", name, ".Encoder ", appliedEncoders]

outputUnion :: Text -> FieldName -> UnionType -> Text
outputUnion name typeTag unionType =
  let caseUnionOutput = outputCaseUnion name (constructorsFrom unionType) typeVariables
      constructorsFrom (PlainUnion constructors) = constructors
      constructorsFrom (GenericUnion _typeVariables constructors) = constructors
      decoderOutput = outputUnionDecoder typeTag name (constructorsFrom unionType)
      caseTypesOutput = outputCaseTypes name typeTag (constructorsFrom unionType)
      caseConstructorOutput = outputCaseConstructors name typeTag (constructorsFrom unionType)
      unionTypeGuardOutput = outputUnionTypeGuard typeTag typeVariables name (constructorsFrom unionType)
      caseTypeGuardOutput = outputCaseTypeGuards name typeTag (constructorsFrom unionType)
      unionValidatorOutput =
        outputUnionValidator typeVariables typeTag name (constructorsFrom unionType)
      caseValidatorOutput = outputCaseValidators typeTag name (constructorsFrom unionType)
      typeVariables = case unionType of
        PlainUnion _constructors -> []
        GenericUnion ts _constructors -> ts
   in Text.intercalate
        "\n\n"
        [ caseUnionOutput,
          decoderOutput
        ]

outputUnionDecoder :: FieldName -> Text -> [Constructor] -> Text
outputUnionDecoder (FieldName tag) unionName constructors =
  let constructorDecodersOutput =
        constructors & fmap (outputConstructorDecoder unionName) & Text.intercalate "\n\n"
      tagAndDecoderOutput =
        constructors
          & fmap
            ( \(Constructor (ConstructorName name) _payload) ->
                mconcat ["                \"", name, "\", ", unionName, ".", name, "Decoder\n"]
            )
          & mconcat
   in mconcat
        [ mconcat [constructorDecodersOutput, "\n\n"],
          mconcat ["    static member Decoder: Decoder<", unionName, "> =\n"],
          mconcat ["        GotynoCoders.decodeWithTypeTag\n"],
          mconcat ["            \"", tag, "\"\n"],
          mconcat ["            [|\n"],
          tagAndDecoderOutput,
          mconcat ["            |]\n"]
        ]

outputConstructorDecoder :: Text -> Constructor -> Text
outputConstructorDecoder unionName (Constructor (ConstructorName name) maybePayload) =
  let decoder = maybe alwaysSucceedingDecoder decoderWithDataField maybePayload
      alwaysSucceedingDecoder = mconcat ["Decode.succeed ", name]
      decoderWithDataField payload =
        mconcat
          [ "Decode.object (fun get -> ",
            name,
            "(get.Required.Field \"data\" ",
            decoderForFieldType payload,
            "))"
          ]
   in mconcat
        [ mconcat ["    static member ", name, "Decoder: Decoder<", unionName, "> =\n"],
          mconcat ["        ", decoder]
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

outputUnionTypeGuard :: FieldName -> [TypeVariable] -> Text -> [Constructor] -> Text
outputUnionTypeGuard typeTag typeVariables unionName constructors =
  if null typeVariables
    then
      let returnExpression _unionName' _tagType constructors' =
            let constructorTypeGuards =
                  constructors'
                    & fmap
                      (\(Constructor (ConstructorName constructorName) _payload) -> "is" <> constructorName)
                    & Text.intercalate ", "
             in mconcat ["[", constructorTypeGuards, "].some((typePredicate) => typePredicate(value))"]
       in outputUnionFunction
            (FunctionPrefix "is")
            (ReturnType ("value is " <>))
            (ReturnExpression returnExpression)
            typeTag
            unionName
            constructors
    else
      let fullName = unionName <> joinTypeVariables typeVariables
          typeVariablePredicates =
            typeVariables
              & fmap (\(TypeVariable t) -> mconcat ["is", t, ": svt.TypePredicate<", t, ">"])
              & Text.intercalate ", "
          returnedFunctionName = "is" <> Text.filter ((`elem` ("<>, " :: String)) >>> not) fullName
          constructorPredicates =
            constructors
              & fmap
                ( \(Constructor (ConstructorName name) maybePayload) ->
                    let constructorTypeVariables =
                          fromMaybe [] $ foldMap typeVariablesFrom maybePayload
                        maybeParameters =
                          if null constructorTypeVariables
                            then ""
                            else "(" <> predicates <> ")"
                        predicates =
                          constructorTypeVariables
                            & fmap (\(TypeVariable t) -> "is" <> t)
                            & Text.intercalate ", "
                     in mconcat ["is", name, maybeParameters]
                )
              & Text.intercalate ", "
       in mconcat
            [ mconcat
                [ "export function is",
                  fullName,
                  "(",
                  typeVariablePredicates,
                  "): svt.TypePredicate<",
                  fullName,
                  "> {\n"
                ],
              mconcat
                [ "    return function ",
                  returnedFunctionName,
                  "(value: unknown): value is ",
                  fullName,
                  " {\n"
                ],
              mconcat
                [ "        return [",
                  constructorPredicates,
                  "].some((typePredicate) => typePredicate(value));\n"
                ],
              "    };\n",
              "}"
            ]

outputUnionValidator :: [TypeVariable] -> FieldName -> Text -> [Constructor] -> Text
outputUnionValidator typeVariables typeTag@(FieldName tag) unionName constructors =
  let constructorTagValidators =
        constructors
          & fmap
            ( \(Constructor (ConstructorName constructorName) maybePayload) ->
                let tagName = unionEnumConstructorTag unionName constructorName
                    constructorTypeVariables = fromMaybe [] $ foldMap typeVariablesFrom maybePayload
                    name =
                      if null constructorTypeVariables
                        then constructorName
                        else
                          mconcat
                            [ constructorName,
                              "(",
                              typeVariableValidatorNames constructorTypeVariables,
                              ")"
                            ]
                 in mconcat ["[", tagName, "]: ", "validate", name]
            )
          & Text.intercalate ", "
   in if null typeVariables
        then
          let returnExpression unionName' (FieldName tag') _constructors' =
                mconcat
                  [ "svt.validateWithTypeTag<",
                    unionName',
                    ">(value, {",
                    constructorTagValidators,
                    "}, \"",
                    tag',
                    "\")"
                  ]
           in outputUnionFunction
                (FunctionPrefix "validate")
                (ReturnType (\n -> "svt.ValidationResult<" <> n <> ">"))
                (ReturnExpression returnExpression)
                typeTag
                unionName
                constructors
        else
          let fullName = unionName <> joinTypeVariables typeVariables
              returnedFunctionName =
                "validate" <> Text.filter ((`elem` ("<>, " :: String)) >>> not) fullName
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
                    [ "        return svt.validateWithTypeTag<",
                      fullName,
                      ">(value, {",
                      constructorTagValidators,
                      "}, \"",
                      tag,
                      "\");\n"
                    ],
                  "    };\n",
                  "}"
                ]

outputCaseTypeGuards :: Text -> FieldName -> [Constructor] -> Text
outputCaseTypeGuards unionName typeTag =
  fmap (outputCaseTypeGuard unionName typeTag) >>> Text.intercalate "\n\n"

outputCaseTypeGuard :: Text -> FieldName -> Constructor -> Text
outputCaseTypeGuard
  unionName
  (FieldName tag)
  (Constructor (ConstructorName name) maybePayload) =
    let tagValue = unionEnumConstructorTag unionName name
        typeVariables = fromMaybe [] $ foldMap typeVariablesFrom maybePayload
        interface =
          mconcat $
            ["{", tag, ": ", tagValue]
              <> maybe ["}"] (\p -> [", data: ", decoderForFieldType p, "}"]) maybePayload
     in if null typeVariables
          then
            mconcat
              [ mconcat ["export function is", name, "(value: unknown): value is ", name, " {\n"],
                mconcat ["    return svt.isInterface<", name, ">(value, ", interface, ");\n"],
                "}"
              ]
          else
            let fullName = name <> joinTypeVariables typeVariables
                returnedFunctionName = "is" <> Text.filter (\c -> c /= '<' && c /= '>') fullName
             in mconcat
                  [ mconcat
                      [ "export function is",
                        fullName,
                        "(",
                        typeVariablePredicateParameters typeVariables,
                        "): svt.TypePredicate<",
                        fullName,
                        "> {\n"
                      ],
                    mconcat
                      [ "    return function ",
                        returnedFunctionName,
                        "(value: unknown): value is ",
                        fullName,
                        " {\n"
                      ],
                    mconcat
                      [ "        return svt.isInterface<",
                        fullName,
                        ">(value, ",
                        interface,
                        ");\n"
                      ],
                    "    };\n",
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
                 in mconcat ["    | ", constructorName, payload, "\n"]
                      <> maybe
                        ""
                        (typeVariablesFrom >>> maybeJoinTypeVariables)
                        maybePayload
            )
          & mconcat
      _maybeTypeVariables = if null typeVariables then "" else joinTypeVariables typeVariables
   in mconcat
        [ mconcat ["type ", name, " =\n"],
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

-- outputCaseConstructorAsStruct :: DefinitionName -> Maybe FieldType -> Text
-- outputCaseConstructorAsStruct name maybePayload =
--   let fields :: [StructField] = maybe [] maybePayload
--    in ""

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
    mconcat [moduleName, ".", name]
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
     in mconcat [moduleName, ".", name, "<", appliedFieldTypes, ">"]

outputBasicType :: BasicTypeValue -> Text
outputBasicType BasicString = "string"
outputBasicType U8 = "number"
outputBasicType U16 = "number"
outputBasicType U32 = "number"
outputBasicType U64 = "number"
outputBasicType U128 = "number"
outputBasicType I8 = "number"
outputBasicType I16 = "number"
outputBasicType I32 = "number"
outputBasicType I64 = "number"
outputBasicType I128 = "number"
outputBasicType F32 = "number"
outputBasicType F64 = "number"
outputBasicType Boolean = "boolean"

maybeJoinTypeVariables :: Maybe [TypeVariable] -> Text
maybeJoinTypeVariables = maybe "" joinTypeVariables

joinTypeVariables :: [TypeVariable] -> Text
joinTypeVariables typeVariables =
  typeVariables & fmap (\(TypeVariable t) -> t) & Text.intercalate ", " & (\o -> "<" <> o <> ">")
