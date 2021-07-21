module CodeGeneration.Python where

import CodeGeneration.Utilities (upperCaseFirstCharacter)
import RIO
import qualified RIO.Text as Text
import Types

outputModule :: Module -> Text
outputModule Module {definitions, imports} =
  let definitionOutput = definitions & mapMaybe outputDefinition & Text.intercalate "\n\n"
      importsOutput = imports & fmap outputImport & mconcat
      outputImport (Import Module {name = ModuleName importName}) =
        mconcat ["from . import ", importName, "\n\n"]
   in mconcat
        [ mconcat
            [ "import json\n",
              "import typing\n",
              "from dataclasses import dataclass\n",
              "from gotyno_validation import validation\n",
              "from gotyno_validation import encoding\n\n"
            ],
          importsOutput,
          definitionOutput
        ]

fsharpifyModuleName :: Text -> Text
fsharpifyModuleName = upperCaseFirstCharacter

outputDefinition :: TypeDefinition -> Maybe Text
outputDefinition (TypeDefinition (DefinitionName name) (Struct (PlainStruct fields))) =
  pure $ outputPlainStruct name fields
outputDefinition (TypeDefinition (DefinitionName name) (Struct (GenericStruct typeVariables fields))) =
  pure $ outputGenericStruct name typeVariables fields
outputDefinition (TypeDefinition (DefinitionName name) (Union typeTag unionType)) =
  pure $ outputUnion name typeTag unionType
outputDefinition (TypeDefinition (DefinitionName name) (Enumeration enumerationValues)) =
  pure $ outputEnumeration name enumerationValues
outputDefinition (TypeDefinition (DefinitionName name) (UntaggedUnion unionCases)) =
  pure $ outputUntaggedUnion name unionCases
outputDefinition (TypeDefinition (DefinitionName name) (EmbeddedUnion typeTag constructors)) =
  pure $ outputEmbeddedUnion name typeTag constructors
outputDefinition (TypeDefinition _name (DeclaredType _moduleName _typeVariables)) = Nothing

outputEmbeddedUnion :: Text -> FieldName -> [EmbeddedConstructor] -> Text
outputEmbeddedUnion _unionName (FieldName _tag) _constructors =
  ""

outputEmbeddedConstructorDecoders :: Text -> [EmbeddedConstructor] -> Text
outputEmbeddedConstructorDecoders unionName =
  fmap (outputEmbeddedConstructorDecoder unionName) >>> Text.intercalate "\n\n"

outputEmbeddedConstructorDecoder :: Text -> EmbeddedConstructor -> Text
outputEmbeddedConstructorDecoder unionName (EmbeddedConstructor (ConstructorName name) reference) =
  let structFields = structFieldsFromReference reference
      structFieldDecoders =
        structFields
          & fmap (outputValidatorForField >>> ("                " <>) >>> (<> "\n"))
          & mconcat
      constructorName = upperCaseFirstCharacter name
   in mconcat
        [ mconcat
            ["    static member ", constructorName, "Decoder: Decoder<", unionName, "> =\n"],
          "        Decode.object (fun get ->\n",
          mconcat ["            ", constructorName, " {\n"],
          structFieldDecoders,
          "            }\n",
          "        )"
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
                mconcat ["        Decode.map ", caseName, " ", validatorForFieldType fieldType]
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
      validatorOutput = outputEnumerationValidator name
      decoderOutput = outputEnumerationDecoder name
      encoderOutput = outputEnumerationEncoder
   in mconcat [typeOutput, "\n\n", validatorOutput, "\n\n", decoderOutput, "\n\n", encoderOutput]

outputEnumerationType :: Text -> [EnumerationValue] -> Text
outputEnumerationType name values =
  let valuesOutput =
        values
          & fmap
            ( \(EnumerationValue (EnumerationIdentifier i) literal) ->
                mconcat ["    ", i, " = ", literalValue literal]
            )
          & Text.intercalate "\n"
      literalValue (LiteralString s) = "'" <> s <> "'"
      literalValue (LiteralInteger i) = tshow i
      literalValue (LiteralFloat f) = tshow f
      literalValue (LiteralBoolean b) = tshow b
   in mconcat [mconcat ["class ", name, "(enum.Enum):\n"], valuesOutput]

fsharpifyConstructorName :: Text -> Text
fsharpifyConstructorName = upperCaseFirstCharacter

outputEnumerationValidator :: Text -> Text
outputEnumerationValidator name =
  mconcat
    [ "    @staticmethod\n",
      mconcat
        [ "    def validate(value: validation.Unknown) -> validation.ValidationResult['",
          name,
          "']:\n"
        ],
      mconcat ["        return validation.validate_enumeration_member(value, ", name, ")"]
    ]

outputEnumerationDecoder :: Text -> Text
outputEnumerationDecoder name =
  mconcat
    [ "    @staticmethod\n",
      mconcat
        [ "    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['",
          name,
          "']:\n"
        ],
      mconcat ["        return validation.validate_from_string(string, ", name, ".validate)"]
    ]

outputEnumerationEncoder :: Text
outputEnumerationEncoder =
  mconcat
    [ "    def to_json(self) -> typing.Any:\n",
      "        return self.value\n\n",
      "    def encode(self) -> str:\n",
      "        return str(self.value)"
    ]

outputPlainStruct :: Text -> [StructField] -> Text
outputPlainStruct name fields =
  let fieldsOutput = fields & fmap (outputField 4) & Text.intercalate "\n"
      validatorOutput = outputStructValidator name fields []
      encoderOutput = outputStructEncoder fields []
      decoderOutput = outputStructDecoder name []
   in mconcat
        [ mconcat ["@dataclass(frozen=True)\nclass ", name, ":\n"],
          fieldsOutput,
          "\n\n",
          validatorOutput,
          "\n\n",
          decoderOutput,
          "\n\n",
          encoderOutput
        ]

outputGenericStruct :: Text -> [TypeVariable] -> [StructField] -> Text
outputGenericStruct name typeVariables fields =
  let fullName = mconcat [name, "(typing.Generic", joinTypeVariables typeVariables, ")"]
      typeOutput =
        mconcat
          [ mconcat ["@dataclass(frozen=True)\nclass ", fullName, ":\n"],
            fieldsOutput
          ]
      fieldsOutput = fields & fmap (outputField 4) & mconcat
      validatorOutput = outputStructValidator name fields typeVariables
      decoderOutput = outputStructDecoder name typeVariables
      encoderOutput = outputStructEncoder fields typeVariables
      typeVariableOutput = typeVariables & fmap outputTypeVariableDefinition & Text.intercalate "\n"
      outputTypeVariableDefinition (TypeVariable t) = mconcat [t, " = typing.TypeVar('", t, "')"]
   in mconcat
        [ typeVariableOutput,
          "\n",
          typeOutput,
          "\n\n",
          validatorOutput,
          "\n\n",
          decoderOutput,
          "\n\n",
          encoderOutput
        ]

outputStructValidator :: Text -> [StructField] -> [TypeVariable] -> Text
outputStructValidator name fields typeVariables =
  let validateFunctionOutput =
        if null typeVariables
          then plainValidator
          else genericValidator
      fullName =
        if null typeVariables
          then name
          else mconcat [name, joinTypeVariables typeVariables]
      plainValidator =
        mconcat
          [ mconcat
              [ "    def validate(value: validation.Unknown) -> validation.ValidationResult['",
                name,
                "']:\n"
              ],
            mconcat
              [ "        return validation.validate_interface(value, ",
                interface,
                ", ",
                name,
                ")"
              ]
          ]
      genericValidator =
        let validatorName =
              mconcat
                [ "validate_",
                  name,
                  typeVariables & fmap (\(TypeVariable t) -> t) & Text.intercalate ""
                ]
         in mconcat
              [ mconcat
                  [ "    def validate(",
                    typeVariableValidatorsAsArguments typeVariables,
                    ") -> validation.Validator['",
                    fullName,
                    "']:\n"
                  ],
                mconcat
                  [ "        def ",
                    validatorName,
                    "(value: validation.Unknown) -> validation.ValidationResult['",
                    fullName,
                    "']:\n"
                  ],
                mconcat
                  [ "            return validation.validate_interface(value, ",
                    interface,
                    ", ",
                    name,
                    ")\n"
                  ],
                mconcat ["        return ", validatorName]
              ]
      interface =
        mconcat ["{", fields & fmap outputValidatorForField & Text.intercalate ", ", "}"]
   in mconcat ["    @staticmethod\n", validateFunctionOutput]

outputStructDecoder :: Text -> [TypeVariable] -> Text
outputStructDecoder name typeVariables =
  let decodeFunctionOutput =
        if null typeVariables
          then plainDecoder
          else genericDecoder
      fullName =
        if null typeVariables
          then name
          else mconcat [name, joinTypeVariables typeVariables]
      plainDecoder =
        mconcat
          [ mconcat
              [ mconcat
                  [ "    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['",
                    name,
                    "']:\n"
                  ]
              ],
            mconcat ["        return validation.validate_from_string(string, ", name, ".validate)"]
          ]
      genericDecoder =
        mconcat
          [ mconcat
              [ "    def decode(string: typing.Union[str, bytes], ",
                validatorArguments,
                ") -> validation.ValidationResult['",
                fullName,
                "']:\n"
              ],
            mconcat
              [ "        return validation.validate_from_string(string, ",
                name,
                ".validate(",
                typeVariables
                  & fmap (TypeVariableReferenceType >>> validatorForFieldType)
                  & Text.intercalate ", ",
                "))"
              ]
          ]
      validatorArguments =
        typeVariableValidatorsAsArguments typeVariables
   in mconcat ["    @staticmethod\n", decodeFunctionOutput]

outputStructEncoder :: [StructField] -> [TypeVariable] -> Text
outputStructEncoder fields typeVariables =
  let interface =
        mconcat
          ["{", fields & fmap outputEncoderForField & Text.intercalate ", ", "}"]
      maybeTypeVariableToJSONArguments =
        if null typeVariables
          then ""
          else
            ", "
              <> ( typeVariables
                     & fmap
                       ( \(TypeVariable t) ->
                           mconcat [t, "_to_json: encoding.ToJSON[", t, "]"]
                       )
                     & Text.intercalate ", "
                 )
      maybePassedInToJSONs =
        if null typeVariables
          then ""
          else typeVariables & fmap (\(TypeVariable t) -> t <> "_to_json") & Text.intercalate ", "
   in mconcat
        [ mconcat ["    def to_json(self", maybeTypeVariableToJSONArguments, ") -> typing.Dict[str, typing.Any]:\n"],
          mconcat ["        return ", interface, "\n\n"],
          mconcat ["    def encode(self", maybeTypeVariableToJSONArguments, ") -> str:\n"],
          mconcat ["        return json.dumps(self.to_json(", maybePassedInToJSONs, "))"]
        ]

outputValidatorForField :: StructField -> Text
outputValidatorForField (StructField (FieldName fieldName) fieldType) =
  mconcat
    [ "'",
      fieldName,
      "': ",
      validatorForFieldType fieldType
    ]

outputEncoderForField :: StructField -> Text
outputEncoderForField
  (StructField (FieldName fieldName) fieldType@(LiteralType _)) =
    mconcat ["'", fieldName, "': ", encoderForFieldType ("", "") fieldType]
outputEncoderForField
  (StructField (FieldName fieldName) (BasicType _)) =
    mconcat ["'", fieldName, "': self.", fieldName]
outputEncoderForField (StructField (FieldName fieldName) fieldType) =
  mconcat
    [ "'",
      fieldName,
      "': ",
      encoderForFieldType ("", "") fieldType,
      "(self.",
      fieldName,
      ")"
    ]

validatorForFieldType :: FieldType -> Text
validatorForFieldType (LiteralType literalType) = validatorForLiteralType literalType
validatorForFieldType (BasicType basicType) = validatorForBasicType basicType
validatorForFieldType (ComplexType complexType) = validatorForComplexType complexType
validatorForFieldType (DefinitionReferenceType definitionReference) =
  decoderForDefinitionReference definitionReference
validatorForFieldType (TypeVariableReferenceType (TypeVariable name)) = "validate_" <> name
validatorForFieldType (RecursiveReferenceType (DefinitionName name)) = name <> ".decode"

validatorForBasicType :: BasicTypeValue -> Text
validatorForBasicType BasicString = "validation.validate_string"
validatorForBasicType U8 = "validation.validate_int"
validatorForBasicType U16 = "validation.validate_int"
validatorForBasicType U32 = "validation.validate_int"
validatorForBasicType U64 = "validation.validate_int"
validatorForBasicType U128 = "validation.validate_int"
validatorForBasicType I8 = "validation.validate_int"
validatorForBasicType I16 = "validation.validate_int"
validatorForBasicType I32 = "validation.validate_int"
validatorForBasicType I64 = "validation.validate_int"
validatorForBasicType I128 = "validation.validate_int"
validatorForBasicType F32 = "validation.validate_float"
validatorForBasicType F64 = "validation.validate_float"
validatorForBasicType Boolean = "validation.validate_bool"

validatorForLiteralType :: LiteralTypeValue -> Text
validatorForLiteralType (LiteralString s) = "validation.validate_literal(\'" <> s <> "\')"
validatorForLiteralType (LiteralInteger i) = "validation.validate_literal(" <> tshow i <> ")"
validatorForLiteralType (LiteralFloat f) = "validation.validate_literal(" <> tshow f <> ")"
validatorForLiteralType (LiteralBoolean b) = "validation.validate_literal(" <> tshow b <> ")"

validatorForComplexType :: ComplexTypeValue -> Text
validatorForComplexType (PointerType fieldType) = validatorForFieldType fieldType
validatorForComplexType (ArrayType _size fieldType) =
  mconcat ["validation.validate_list(", validatorForFieldType fieldType, ")"]
validatorForComplexType (SliceType fieldType) =
  mconcat ["validation.validate_list(", validatorForFieldType fieldType, ")"]
validatorForComplexType (OptionalType fieldType) =
  mconcat ["validation.validate_optional(", validatorForFieldType fieldType, ")"]

decoderForDefinitionReference :: DefinitionReference -> Text
decoderForDefinitionReference
  ( DefinitionReference
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    name <> ".validate"
decoderForDefinitionReference
  ( ImportedDefinitionReference
      (ModuleName moduleName)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    mconcat [fsharpifyModuleName moduleName, ".", name, ".validate"]
decoderForDefinitionReference
  ( AppliedGenericReference
      appliedTypes
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedDecoders = appliedTypes & fmap validatorForFieldType & Text.intercalate " "
     in mconcat ["(", name, ".validate ", appliedDecoders, ")"]
decoderForDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedDecoders = appliedTypes & fmap validatorForFieldType & Text.intercalate " "
     in mconcat ["(", fsharpifyModuleName moduleName, ".", name, ".validate ", appliedDecoders, ")"]
decoderForDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedDecoders = appliedTypes & fmap validatorForFieldType & Text.intercalate " "
     in mconcat ["(", fsharpifyModuleName moduleName, ".", name, ".validate ", appliedDecoders, ")"]
decoderForDefinitionReference
  (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
    mconcat [fsharpifyModuleName moduleName, ".", name, ".validate"]

encoderForFieldType :: (Text, Text) -> FieldType -> Text
encoderForFieldType (_l, _r) (LiteralType literalType) = encoderForLiteralType literalType
encoderForFieldType (_l, _r) (BasicType basicType) = encoderForBasicType basicType
encoderForFieldType (l, r) (ComplexType complexType) = l <> encoderForComplexType complexType <> r
encoderForFieldType (_l, _r) (DefinitionReferenceType definitionReference) =
  encoderForDefinitionReference definitionReference
encoderForFieldType (_l, _r) (TypeVariableReferenceType (TypeVariable name)) = name <> "_to_json"
encoderForFieldType (_l, _r) (RecursiveReferenceType (DefinitionName name)) = name <> ".to_json"

encoderForBasicType :: BasicTypeValue -> Text
encoderForBasicType _ = "encoding.basic_to_json"

encoderForLiteralType :: LiteralTypeValue -> Text
encoderForLiteralType (LiteralString s) = "'" <> s <> "'"
encoderForLiteralType (LiteralInteger i) = tshow i
encoderForLiteralType (LiteralFloat f) = tshow f
encoderForLiteralType (LiteralBoolean b) = tshow b

encoderForComplexType :: ComplexTypeValue -> Text
encoderForComplexType (PointerType fieldType) = encoderForFieldType ("", "") fieldType
encoderForComplexType (ArrayType _size fieldType) =
  mconcat ["encoding.list_to_json(", encoderForFieldType ("(", ")") fieldType, ")"]
encoderForComplexType (SliceType fieldType) =
  mconcat ["encoding.list_to_json(", encoderForFieldType ("(", ")") fieldType, ")"]
encoderForComplexType (OptionalType fieldType) =
  mconcat ["encoding.optional_to_json(", encoderForFieldType ("(", ")") fieldType, ")"]

encoderForDefinitionReference :: DefinitionReference -> Text
encoderForDefinitionReference
  ( DefinitionReference
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    name <> ".to_json"
encoderForDefinitionReference
  ( ImportedDefinitionReference
      (ModuleName moduleName)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    mconcat [moduleName, ".", name, ".to_json"]
encoderForDefinitionReference
  ( AppliedGenericReference
      appliedTypes
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedEncoders = appliedTypes & fmap (encoderForFieldType ("", "")) & Text.intercalate " "
     in mconcat [name, ".to_json(", appliedEncoders, ")"]
encoderForDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedEncoders = appliedTypes & fmap (encoderForFieldType ("", "")) & Text.intercalate " "
     in mconcat [moduleName, ".", name, ".to_json(", appliedEncoders, ")"]
encoderForDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedEncoders = appliedTypes & fmap (encoderForFieldType ("", "")) & Text.intercalate " "
     in mconcat [moduleName, ".", name, ".to_json(", appliedEncoders, ")"]
encoderForDefinitionReference
  (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
    mconcat [moduleName, ".", name, ".to_json"]

outputUnion :: Text -> FieldName -> UnionType -> Text
outputUnion name typeTag unionType =
  let baseClassOutput = outputUnionBaseClass name typeTag (constructorsFrom unionType) typeVariables
      casesOutput = outputUnionCases fullUnionName typeTag (constructorsFrom unionType)
      fullUnionName =
        if null typeVariables
          then name
          else mconcat [name, joinTypeVariables typeVariables]
      constructorsFrom (PlainUnion constructors) = constructors
      constructorsFrom (GenericUnion _typeVariables constructors) = constructors
      typeVariables = case unionType of
        PlainUnion _constructors -> []
        GenericUnion ts _constructors -> ts
      maybeTypeVariableOutput =
        if null typeVariables
          then ""
          else
            typeVariables
              & fmap (\(TypeVariable t) -> mconcat [t, " = typing.TypeVar('", t, "')"])
              & Text.intercalate "\n"
              & (<> "\n")
   in mconcat [maybeTypeVariableOutput, baseClassOutput, "\n\n", casesOutput]

outputUnionBaseClass :: Text -> FieldName -> [Constructor] -> [TypeVariable] -> Text
outputUnionBaseClass name tag constructors typeVariables =
  let validatorOutput = outputUnionValidator name tag constructors typeVariables
      decoderOutput = outputUnionDecoder name typeVariables
      maybeGenericNotation =
        if null typeVariables
          then ""
          else mconcat ["(typing.Generic", joinTypeVariables typeVariables, ")"]
      stubsOutput =
        mconcat
          [ "    def to_json(self) -> typing.Dict[str, typing.Any]:\n",
            mconcat
              [ "        raise NotImplementedError('`to_json` is not implemented for base class `",
                name,
                "`')\n\n"
              ],
            "    def encode(self) -> str:\n",
            mconcat
              [ "        raise NotImplementedError('`encode` is not implemented for base class `",
                name,
                "`')"
              ]
          ]
   in mconcat
        [ mconcat ["class ", name, maybeGenericNotation, ":\n"],
          validatorOutput,
          "\n\n",
          decoderOutput,
          "\n\n",
          stubsOutput
        ]

outputUnionValidator :: Text -> FieldName -> [Constructor] -> [TypeVariable] -> Text
outputUnionValidator name (FieldName tag) constructors typeVariables =
  let taggedValidators =
        constructors
          & fmap
            ( \(Constructor (ConstructorName constructorName) maybePayload) ->
                let payloadTypeVariables =
                      fromMaybe [] $ foldMap typeVariablesFrom maybePayload
                    maybeTypeVariableValidatorArguments =
                      if null payloadTypeVariables
                        then ""
                        else
                          mconcat
                            [ "(",
                              payloadTypeVariables
                                & fmap (\(TypeVariable t) -> "validate_" <> t)
                                & Text.intercalate
                                  ", ",
                              ")"
                            ]
                 in mconcat
                      [ "'",
                        constructorName,
                        "': ",
                        constructorName,
                        ".validate",
                        maybeTypeVariableValidatorArguments
                      ]
            )
          & Text.intercalate ", "
      interface = "{" <> taggedValidators <> "}"
      functionHead [] =
        mconcat ["    def validate(value: validation.Unknown) -> validation.ValidationResult['", name, "']:\n"]
      functionHead typeVariables' =
        mconcat
          [ "    def validate(",
            typeVariableValidatorsAsArguments typeVariables',
            ") -> validation.Validator['",
            fullName,
            "']:\n"
          ]
      functionBody [] =
        mconcat ["        return validation.validate_with_type_tags(value, '", tag, "', ", interface, ")"]
      functionBody typeVariables' =
        let validatorName =
              "validate_" <> name <> (typeVariables' & fmap (\(TypeVariable t) -> t) & mconcat)
         in mconcat
              [ mconcat
                  [ "        def ",
                    validatorName,
                    "(value: validation.Unknown) -> validation.ValidationResult['",
                    fullName,
                    "']:\n"
                  ],
                mconcat
                  [ "            return validation.validate_with_type_tags(value, '",
                    tag,
                    "', ",
                    interface,
                    ")\n"
                  ],
                mconcat ["        return ", validatorName]
              ]
      fullName =
        if null typeVariables
          then name
          else mconcat [name, joinTypeVariables typeVariables]
   in mconcat
        [ "    @staticmethod\n",
          functionHead typeVariables,
          functionBody typeVariables
        ]

outputUnionDecoder :: Text -> [TypeVariable] -> Text
outputUnionDecoder unionName typeVariables =
  let maybeTypeVariableValidatorArguments =
        if null typeVariables
          then ""
          else
            mconcat
              [ ", ",
                typeVariables
                  & fmap (\(TypeVariable t) -> mconcat ["validate_" <> t, ": validation.Validator[", t, "]"])
                  & Text.intercalate
                    ", "
              ]
      maybePassedInValidators =
        if null typeVariables
          then ""
          else
            mconcat
              [ "(",
                typeVariables
                  & fmap (\(TypeVariable t) -> "validate_" <> t)
                  & Text.intercalate ", ",
                ")"
              ]
      fullName =
        if null typeVariables
          then unionName
          else mconcat [unionName, joinTypeVariables typeVariables]
   in mconcat
        [ "    @staticmethod\n",
          mconcat
            [ "    def decode(string: typing.Union[str, bytes]",
              maybeTypeVariableValidatorArguments,
              ") -> validation.ValidationResult['",
              fullName,
              "']:\n"
            ],
          mconcat
            [ "        return validation.validate_from_string(string, ",
              unionName,
              ".validate",
              maybePassedInValidators,
              ")"
            ]
        ]

outputUnionCases :: Text -> FieldName -> [Constructor] -> Text
outputUnionCases unionName tag =
  fmap (outputUnionCase unionName tag) >>> Text.intercalate "\n\n"

outputUnionCase :: Text -> FieldName -> Constructor -> Text
outputUnionCase
  unionName
  fieldName@(FieldName tag)
  constructor@(Constructor (ConstructorName name) maybePayload) =
    let payloadTypeVariables =
          fromMaybe [] $ foldMap typeVariablesFrom maybePayload
        fullName =
          if null payloadTypeVariables
            then name
            else mconcat [name, joinTypeVariables payloadTypeVariables]
        maybeDataField =
          maybePayload & maybe "" (outputFieldType >>> ("    data: " <>) >>> (<> "\n\n"))
        interface =
          mconcat ["{", maybePayload & maybe "" (validatorForFieldType >>> ("'data': " <>)), "}"]
        validatorOutput =
          mconcat
            [ "    @staticmethod\n",
              validatorFunctionHead payloadTypeVariables,
              validatorFunctionBody payloadTypeVariables
            ]
        validatorFunctionHead [] =
          mconcat ["    def validate(value: validation.Unknown) -> validation.ValidationResult['", name, "']:\n"]
        validatorFunctionHead typeVariables =
          mconcat
            [ "    def validate(",
              typeVariableValidatorsAsArguments typeVariables,
              ") -> validation.Validator['",
              fullName,
              "']:\n"
            ]
        validatorFunctionBody [] =
          mconcat
            [ "        return validation.validate_with_type_tag(value, '",
              tag,
              "', '",
              name,
              "', ",
              interface,
              ", ",
              name,
              ")"
            ]
        validatorFunctionBody typeVariables =
          let validatorName =
                "validate_" <> name <> (typeVariables & fmap (\(TypeVariable t) -> t) & mconcat)
           in mconcat
                [ mconcat
                    [ "        def ",
                      validatorName,
                      "(value: validation.Unknown) -> validation.ValidationResult['",
                      fullName,
                      "']:\n"
                    ],
                  mconcat
                    [ "            return validation.validate_with_type_tag(value, '",
                      tag,
                      "', '",
                      name,
                      "', ",
                      interface,
                      ", ",
                      name,
                      ")\n"
                    ],
                  mconcat ["        return ", validatorName]
                ]
        decoderOutput =
          let maybeTypeVariableValidatorArguments =
                if null payloadTypeVariables
                  then ""
                  else
                    mconcat
                      [ ", ",
                        payloadTypeVariables
                          & fmap (\(TypeVariable t) -> mconcat ["validate_" <> t, ": validation.Validator[", t, "]"])
                          & Text.intercalate ", "
                      ]
              maybePassedInValidators =
                if null payloadTypeVariables
                  then ""
                  else
                    mconcat
                      [ "(",
                        payloadTypeVariables
                          & fmap (\(TypeVariable t) -> "validate_" <> t)
                          & Text.intercalate ", ",
                        ")"
                      ]
           in mconcat
                [ "    @staticmethod\n",
                  mconcat
                    [ "    def decode(string: typing.Union[str, bytes]",
                      maybeTypeVariableValidatorArguments,
                      ") -> validation.ValidationResult['",
                      fullName,
                      "']:\n"
                    ],
                  mconcat
                    [ "        return validation.validate_from_string(string, ",
                      name,
                      ".validate",
                      maybePassedInValidators,
                      ")"
                    ]
                ]
        encoderOutput = outputEncoderForUnionConstructor fieldName constructor
     in mconcat
          [ "@dataclass(frozen=True)\n",
            mconcat ["class ", name, "(", unionName, "):\n"],
            maybeDataField,
            validatorOutput,
            "\n\n",
            decoderOutput,
            "\n\n",
            encoderOutput
          ]

outputEncoderForUnionConstructor :: FieldName -> Constructor -> Text
outputEncoderForUnionConstructor (FieldName tag) (Constructor (ConstructorName name) maybePayload) =
  let payloadTypeVariables =
        fromMaybe [] $ foldMap typeVariablesFrom maybePayload
      maybeDataField =
        maybe "" (dataEncoder >>> (", 'data': " <>)) maybePayload
      dataEncoder (BasicType _) = "self.data"
      dataEncoder (DefinitionReferenceType _) = "self.data.to_json()"
      dataEncoder fieldType = mconcat [encoderForFieldType ("", "") fieldType, "(self.data)"]
      maybeTypeVariableToJSONArguments =
        if null payloadTypeVariables
          then ""
          else
            mconcat
              [ ", ",
                payloadTypeVariables
                  & fmap
                    ( \(TypeVariable t) ->
                        mconcat [t, "_to_json: encoding.ToJSON[", t, "]"]
                    )
                  & Text.intercalate ", "
              ]
      interface = mconcat ["{", mconcat ["'", tag, "': '", name, "'", maybeDataField], "}"]
      maybePassedInToJSONs =
        if null payloadTypeVariables
          then ""
          else
            payloadTypeVariables
              & fmap (\(TypeVariable t) -> t <> "_to_json")
              & Text.intercalate ", "
   in mconcat
        [ mconcat ["    def to_json(self", maybeTypeVariableToJSONArguments, ") -> typing.Dict[str, typing.Any]:\n"],
          mconcat ["        return ", interface, "\n\n"],
          mconcat ["    def encode(self", maybeTypeVariableToJSONArguments, ") -> str:\n"],
          mconcat ["        return json.dumps(self.to_json(", maybePassedInToJSONs, "))"]
        ]

typeVariableValidatorsAsArguments :: [TypeVariable] -> Text
typeVariableValidatorsAsArguments [] = ""
typeVariableValidatorsAsArguments typeVariables =
  let types = fmap (\(TypeVariable t) -> mconcat ["validation.Validator[", t, "]"]) typeVariables
   in typeVariables
        & validatorsForTypeVariables
        & zip types
        & fmap (\(t, v) -> v <> ": " <> t)
        & Text.intercalate ", "

validatorsForTypeVariables :: [TypeVariable] -> [Text]
validatorsForTypeVariables = fmap (TypeVariableReferenceType >>> validatorForFieldType)

typeVariableEncodersAsArguments :: [TypeVariable] -> Text
typeVariableEncodersAsArguments [] = ""
typeVariableEncodersAsArguments typeVariables =
  " " <> (encodersForTypeVariables typeVariables & Text.intercalate " ")

encodersForTypeVariables :: [TypeVariable] -> [Text]
encodersForTypeVariables = fmap (TypeVariableReferenceType >>> encoderForFieldType ("", ""))

outputConstructorDecoder :: Text -> [TypeVariable] -> Constructor -> Text
outputConstructorDecoder unionName typeVariables (Constructor (ConstructorName name) maybePayload) =
  let decoder = maybe alwaysSucceedingDecoder decoderWithDataField maybePayload
      constructorName = upperCaseFirstCharacter name
      alwaysSucceedingDecoder = mconcat ["Decode.succeed ", constructorName]
      payloadTypeVariables = fromMaybe [] $ foldMap typeVariablesFrom maybePayload
      maybeArguments = typeVariableValidatorsAsArguments payloadTypeVariables
      fullName =
        if null typeVariables
          then unionName
          else mconcat [unionName, joinTypeVariables typeVariables]
      decoderWithDataField payload =
        mconcat
          [ "Decode.object (fun get -> ",
            constructorName,
            "(get.Required.Field \"data\" ",
            validatorForFieldType payload,
            "))"
          ]
   in mconcat
        [ mconcat
            [ "    static member ",
              constructorName,
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
        [ mconcat ["        | ", upperCaseFirstCharacter name, maybePayloadPart, " ->\n"],
          mconcat ["            Encode.object ", interface]
        ]

outputCaseUnion :: Text -> [Constructor] -> [TypeVariable] -> Text
outputCaseUnion name constructors typeVariables =
  let cases =
        constructors
          & fmap
            ( \(Constructor (ConstructorName constructorName) maybePayload) ->
                let payload = maybe "" (outputFieldType >>> (" of " <>)) maybePayload
                 in mconcat ["    | ", upperCaseFirstCharacter constructorName, payload]
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
typeVariablesFromReference
  ( GenericDeclarationReference
      _moduleName
      _definitionName
      (AppliedTypes fieldTypes)
    ) =
    let typeVariables = fieldTypes & fmap typeVariablesFrom & catMaybes & join
     in if null typeVariables then Nothing else Just typeVariables
typeVariablesFromReference (DeclarationReference _moduleName _definitionName) =
  Nothing

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
typeVariablesFromDefinition (TypeDefinition _name (DeclaredType _moduleName typeVariables)) =
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
   in indent <> mconcat [name, ": ", outputFieldType fieldType]

sanitizeName :: Text -> Text
sanitizeName "type" = "``type``"
sanitizeName "private" = "``private``"
sanitizeName "class" = "``class``"
sanitizeName "public" = "``public``"
sanitizeName name = name

outputFieldType :: FieldType -> Text
outputFieldType (LiteralType (LiteralString text)) = "typing.Literal['" <> text <> "']"
outputFieldType (LiteralType (LiteralInteger x)) = "typing.Literal['" <> tshow x <> "']"
outputFieldType (LiteralType (LiteralFloat f)) = "typing.Literal['" <> tshow f <> "']"
outputFieldType (LiteralType (LiteralBoolean b)) = "typing.Literal['" <> tshow b <> "']"
outputFieldType (BasicType basicType) = outputBasicType basicType
outputFieldType (ComplexType (OptionalType fieldType)) =
  mconcat ["typing.Optional[", outputFieldType fieldType, "]"]
outputFieldType (ComplexType (ArrayType _size fieldType)) =
  mconcat ["typing.List[", outputFieldType fieldType, "]"]
outputFieldType (ComplexType (SliceType fieldType)) =
  mconcat ["typing.List[", outputFieldType fieldType, "]"]
outputFieldType (ComplexType (PointerType fieldType)) = outputFieldType fieldType
outputFieldType (RecursiveReferenceType (DefinitionName name)) = name
outputFieldType (DefinitionReferenceType definitionReference) =
  outputDefinitionReference definitionReference
outputFieldType (TypeVariableReferenceType (TypeVariable t)) = t

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
outputDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate ", "
        maybeAppliedOutput = if null appliedTypes then "" else mconcat ["<", appliedFieldTypes, ">"]
     in mconcat [fsharpifyModuleName moduleName, ".", name, maybeAppliedOutput]
outputDefinitionReference (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
  mconcat [fsharpifyModuleName moduleName, ".", name]

outputBasicType :: BasicTypeValue -> Text
outputBasicType BasicString = "str"
outputBasicType U8 = "int"
outputBasicType U16 = "int"
outputBasicType U32 = "int"
outputBasicType U64 = "int"
outputBasicType U128 = "int"
outputBasicType I8 = "int"
outputBasicType I16 = "int"
outputBasicType I32 = "int"
outputBasicType I64 = "int"
outputBasicType I128 = "int"
outputBasicType F32 = "float"
outputBasicType F64 = "float"
outputBasicType Boolean = "bool"

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

maybeJoinTypeVariables :: Maybe [TypeVariable] -> Text
maybeJoinTypeVariables = maybe "" joinTypeVariables

joinTypeVariables :: [TypeVariable] -> Text
joinTypeVariables typeVariables =
  typeVariables
    & fmap (\(TypeVariable t) -> t)
    & Text.intercalate ", "
    & (\o -> "[" <> o <> "]")
