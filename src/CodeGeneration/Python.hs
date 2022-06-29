module CodeGeneration.Python (outputModule) where

import CodeGeneration.Utilities (typeVariablesFrom, upperCaseFirstCharacter)
import RIO
import qualified RIO.Text as Text
import Types

outputModule :: Module -> Text
outputModule Module {definitions, imports, declarationNames} =
  let definitionOutput = definitions & mapMaybe outputDefinition & Text.intercalate "\n\n"
      importsOutput = imports & fmap outputImport & Text.intercalate "\n"
      outputImport (Import Module {name = ModuleName importName}) =
        mconcat ["from . import ", importName]
      declarationImportsOutput =
        declarationNames
          & fmap
            ( \(ModuleName name) ->
                mconcat ["from . import ", name]
            )
          & Text.intercalate "\n"
   in mconcat
        [ mconcat
            [ "import enum\n",
              "import json\n",
              "import typing\n",
              "from dataclasses import dataclass\n",
              "from gotyno_validation import encoding, validation\n\n"
            ],
          importsOutput,
          if null imports then "" else "\n\n",
          declarationImportsOutput,
          if null declarationNames then "" else "\n\n",
          definitionOutput
        ]

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
outputEmbeddedUnion unionName fieldName constructors =
  let baseClassOutput =
        outputUnionBaseClass
          unionName
          fieldName
          (embeddedConstructorsToConstructors constructors)
          []
      casesOutput = outputEmbeddedUnionCases unionName fieldName constructors
   in mconcat [baseClassOutput, "\n\n", casesOutput]

outputEmbeddedUnionCases :: Text -> FieldName -> [EmbeddedConstructor] -> Text
outputEmbeddedUnionCases unionName fieldName =
  fmap (outputEmbeddedUnionCase unionName fieldName) >>> Text.intercalate "\n\n"

outputEmbeddedUnionCase :: Text -> FieldName -> EmbeddedConstructor -> Text
outputEmbeddedUnionCase
  unionName
  tag
  constructor@(EmbeddedConstructor (ConstructorName name) Nothing) =
    let validatorOutput = outputEmbeddedConstructorDecoder tag constructor
        encoderOutput = outputEmbeddedConstructorEncoder tag constructor
     in mconcat
          [ "@dataclass\n",
            mconcat ["class ", upperCaseFirstCharacter name, "(", unionName, "):\n"],
            validatorOutput,
            "\n\n",
            encoderOutput
          ]
outputEmbeddedUnionCase
  unionName
  tag
  constructor@(EmbeddedConstructor (ConstructorName name) (Just reference)) =
    let structFields = structFieldsFromReference reference
        typesOutput =
          structFields
            & fmap
              ( \(StructField (FieldName n) fieldType) ->
                  mconcat ["    ", n, ": ", outputFieldType fieldType]
              )
            & Text.intercalate "\n"
        validatorOutput = outputEmbeddedConstructorDecoder tag constructor
        encoderOutput = outputEmbeddedConstructorEncoder tag constructor
     in mconcat
          [ "@dataclass\n",
            mconcat ["class ", upperCaseFirstCharacter name, "(", unionName, "):\n"],
            typesOutput,
            "\n\n",
            validatorOutput,
            "\n\n",
            encoderOutput
          ]

outputEmbeddedConstructorDecoder :: FieldName -> EmbeddedConstructor -> Text
outputEmbeddedConstructorDecoder
  (FieldName tag)
  (EmbeddedConstructor (ConstructorName name) Nothing) =
    let typeName = upperCaseFirstCharacter name
     in mconcat
          [ "    @staticmethod\n",
            mconcat ["    def validate(value: validation.Unknown) -> validation.ValidationResult['", typeName, "']:\n"],
            mconcat
              [ "        return validation.validate_with_type_tag_and_validator(value, '",
                tag,
                "', '",
                name,
                "', validation.validate_unknown, ",
                typeName,
                ")\n\n"
              ],
            "    @staticmethod\n",
            mconcat
              [ "    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['",
                typeName,
                "']:\n"
              ],
            mconcat ["        return validation.validate_from_string(string, ", typeName, ".validate)"]
          ]
outputEmbeddedConstructorDecoder
  (FieldName tag)
  (EmbeddedConstructor (ConstructorName name) (Just reference)) =
    let typeName = upperCaseFirstCharacter name
        DefinitionName referenceName = nameOfReference reference
     in mconcat
          [ "    @staticmethod\n",
            mconcat ["    def validate(value: validation.Unknown) -> validation.ValidationResult['", typeName, "']:\n"],
            mconcat
              [ "        return validation.validate_with_type_tag_and_validator(value, '",
                tag,
                "', '",
                name,
                "', ",
                referenceName,
                ".validate, ",
                typeName,
                ")\n\n"
              ],
            "    @staticmethod\n",
            mconcat
              [ "    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['",
                typeName,
                "']:\n"
              ],
            mconcat ["        return validation.validate_from_string(string, ", typeName, ".validate)"]
          ]

outputEmbeddedConstructorEncoder :: FieldName -> EmbeddedConstructor -> Text
outputEmbeddedConstructorEncoder
  (FieldName tag)
  (EmbeddedConstructor (ConstructorName name) Nothing) =
    let interface = mconcat ["{'", tag, "': '", name, "'}"]
     in mconcat
          [ "    def to_json(self) -> typing.Dict[str, typing.Any]:\n",
            mconcat ["        return ", interface, "\n\n"],
            "    def encode(self) -> str:\n",
            "        return json.dumps(self.to_json())"
          ]
outputEmbeddedConstructorEncoder
  (FieldName tag)
  (EmbeddedConstructor (ConstructorName name) (Just reference)) =
    let DefinitionName referenceName = nameOfReference reference
        interface = mconcat ["{'", tag, "': '", name, "', **", referenceName, ".to_json(self)}"]
     in mconcat
          [ "    def to_json(self) -> typing.Dict[str, typing.Any]:\n",
            mconcat ["        return ", interface, "\n\n"],
            "    def encode(self) -> str:\n",
            "        return json.dumps(self.to_json())"
          ]

structFieldsFromReference :: DefinitionReference -> [StructField]
structFieldsFromReference
  (DefinitionReference (TypeDefinition _name (Struct (PlainStruct fields)))) = fields
structFieldsFromReference _other = error "struct fields from anything other than plain struct"

embeddedConstructorsToConstructors :: [EmbeddedConstructor] -> [Constructor]
embeddedConstructorsToConstructors = fmap embeddedConstructorToConstructor

embeddedConstructorToConstructor :: EmbeddedConstructor -> Constructor
embeddedConstructorToConstructor (EmbeddedConstructor name reference) =
  Constructor name (DefinitionReferenceType <$> reference)

outputUntaggedUnion :: Text -> [FieldType] -> Text
outputUntaggedUnion unionName cases =
  let typeOutput = mconcat [unionName, " = ", "typing.Union[", unionOutput, "]"]
      unionOutput = cases & fmap outputFieldType & Text.intercalate ", "
      interfaceOutput = outputUntaggedUnionInterface unionName cases
   in mconcat [typeOutput, "\n", interfaceOutput]

outputUntaggedUnionInterface :: Text -> [FieldType] -> Text
outputUntaggedUnionInterface unionName cases =
  let oneOfValidatorsOutput =
        mconcat
          [ "[",
            cases
              & fmap validatorForFieldType
              & Text.intercalate ", ",
            "]"
          ]
      oneOfToJSONInterface =
        mconcat
          [ "{",
            cases
              & fmap
                ( \fieldType ->
                    fieldTypeName fieldType <> ": " <> encoderForFieldType fieldType
                )
              & Text.intercalate ", ",
            "}"
          ]
   in mconcat
        [ mconcat ["class ", unionName, "Interface:\n"],
          "    @staticmethod\n",
          mconcat
            [ "    def validate(value: validation.Unknown) -> validation.ValidationResult['",
              unionName,
              "']:\n"
            ],
          mconcat
            [ "        return validation.validate_one_of(value, ",
              oneOfValidatorsOutput,
              ")\n\n"
            ],
          "    @staticmethod\n",
          mconcat
            [ "    def decode(string: typing.Union[str, bytes]) -> validation.ValidationResult['",
              unionName,
              "']:\n"
            ],
          mconcat
            [ "        return validation.validate_from_string(string, ",
              unionName,
              "Interface.validate)\n\n"
            ],
          "    @staticmethod\n",
          "    def to_json(value) -> typing.Any:\n",
          mconcat
            ["        return encoding.one_of_to_json(value, ", oneOfToJSONInterface, ")\n\n"],
          "    @staticmethod\n",
          "    def encode(value) -> str:\n",
          "        return json.dumps(value.to_json())"
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
      literalValue (LiteralString s) = "\"" <> s <> "\""
      literalValue (LiteralInteger i) = tshow i
      literalValue (LiteralFloat f) = tshow f
      literalValue (LiteralBoolean b) = tshow b
   in mconcat [mconcat ["class ", name, "(enum.Enum):\n"], valuesOutput]

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
      encoderOutput = outputStructEncoder fields
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
      fieldsOutput = fields & fmap (outputField 4) & Text.intercalate "\n"
      validatorOutput = outputStructValidator name fields typeVariables
      decoderOutput = outputStructDecoder name typeVariables
      encoderOutput = outputStructEncoder fields
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

outputStructEncoder :: [StructField] -> Text
outputStructEncoder fields =
  let interface = mconcat ["{", fields & fmap outputEncoderForField & Text.intercalate ", ", "}"]
   in mconcat
        [ mconcat ["    def to_json(self) -> typing.Dict[str, typing.Any]:\n"],
          mconcat ["        return ", interface, "\n\n"],
          mconcat ["    def encode(self) -> str:\n"],
          mconcat ["        return json.dumps(self.to_json())"]
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
    mconcat ["'", fieldName, "': ", encoderForFieldType fieldType]
outputEncoderForField
  (StructField (FieldName fieldName) basicType@(BasicType t))
    | t `elem` [U64, U128, I64, I128] =
      mconcat
        [ "'",
          fieldName,
          "': ",
          encoderForFieldType basicType,
          "(self.",
          fieldName,
          ")"
        ]
    | otherwise = mconcat ["'", fieldName, "': self.", fieldName]
outputEncoderForField
  ( StructField
      (FieldName fieldName)
      (DefinitionReferenceType (AppliedGenericReference _appliedTypeVariables _definition))
    ) =
    mconcat ["'", fieldName, "': self.", fieldName, ".to_json()"]
outputEncoderForField
  ( StructField
      (FieldName fieldName)
      ( DefinitionReferenceType
          ( AppliedImportedGenericReference
              _moduleName
              _appliedTypes
              _definition
            )
        )
    ) =
    mconcat ["'", fieldName, "': self.", fieldName, ".to_json()"]
outputEncoderForField (StructField (FieldName fieldName) fieldType) =
  mconcat
    [ "'",
      fieldName,
      "': ",
      encoderForFieldType fieldType,
      "(self.",
      fieldName,
      ")"
    ]

encoderForFieldType :: FieldType -> Text
encoderForFieldType (LiteralType literalType) = encoderForLiteralType literalType
encoderForFieldType (BasicType basicType) = encoderForBasicType basicType
encoderForFieldType (ComplexType complexType) = encoderForComplexType complexType
encoderForFieldType (DefinitionReferenceType definitionReference) =
  encoderForDefinitionReference definitionReference
encoderForFieldType (TypeVariableReferenceType (TypeVariable _name)) = "encoding.general_to_json"
encoderForFieldType (RecursiveReferenceType (DefinitionName name)) = name <> ".to_json"

encoderForBasicType :: BasicTypeValue -> Text
encoderForBasicType U64 = "encoding.bigint_to_json"
encoderForBasicType U128 = "encoding.bigint_to_json"
encoderForBasicType I64 = "encoding.bigint_to_json"
encoderForBasicType I128 = "encoding.bigint_to_json"
encoderForBasicType _ = "encoding.basic_to_json"

encoderForLiteralType :: LiteralTypeValue -> Text
encoderForLiteralType (LiteralString s) = "'" <> s <> "'"
encoderForLiteralType (LiteralInteger i) = tshow i
encoderForLiteralType (LiteralFloat f) = tshow f
encoderForLiteralType (LiteralBoolean b) = tshow b

encoderForComplexType :: ComplexTypeValue -> Text
encoderForComplexType (PointerType fieldType) = encoderForFieldType fieldType
encoderForComplexType (ArrayType _size fieldType) =
  mconcat ["encoding.list_to_json(", encoderForFieldType fieldType, ")"]
encoderForComplexType (SliceType fieldType) =
  mconcat ["encoding.list_to_json(", encoderForFieldType fieldType, ")"]
encoderForComplexType (OptionalType fieldType) =
  mconcat ["encoding.optional_to_json(", encoderForFieldType fieldType, ")"]

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
    let appliedEncoders = appliedTypes & fmap encoderForFieldType & Text.intercalate " "
     in mconcat [name, ".to_json(", appliedEncoders, ")"]
encoderForDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedEncoders = appliedTypes & fmap encoderForFieldType & Text.intercalate " "
     in mconcat [moduleName, ".", name, ".to_json(", appliedEncoders, ")"]
encoderForDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedEncoders = appliedTypes & fmap encoderForFieldType & Text.intercalate " "
     in mconcat [moduleName, ".", name, ".to_json(", appliedEncoders, ")"]
encoderForDefinitionReference
  (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
    mconcat [moduleName, ".", name, ".to_json"]

validatorForFieldType :: FieldType -> Text
validatorForFieldType (LiteralType literalType) = validatorForLiteralType literalType
validatorForFieldType (BasicType basicType) = validatorForBasicType basicType
validatorForFieldType (ComplexType complexType) = validatorForComplexType complexType
validatorForFieldType (DefinitionReferenceType definitionReference) =
  decoderForDefinitionReference definitionReference
validatorForFieldType (TypeVariableReferenceType (TypeVariable name)) = "validate_" <> name
validatorForFieldType (RecursiveReferenceType (DefinitionName name)) = name <> ".validate"

validatorForBasicType :: BasicTypeValue -> Text
validatorForBasicType BasicString = "validation.validate_string"
validatorForBasicType U8 = "validation.validate_int"
validatorForBasicType U16 = "validation.validate_int"
validatorForBasicType U32 = "validation.validate_int"
validatorForBasicType U64 = "validation.validate_bigint"
validatorForBasicType U128 = "validation.validate_bigint"
validatorForBasicType I8 = "validation.validate_int"
validatorForBasicType I16 = "validation.validate_int"
validatorForBasicType I32 = "validation.validate_int"
validatorForBasicType I64 = "validation.validate_bigint"
validatorForBasicType I128 = "validation.validate_bigint"
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
    mconcat [moduleName, ".", name, ".validate"]
decoderForDefinitionReference
  ( AppliedGenericReference
      appliedTypes
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedDecoders = appliedTypes & fmap validatorForFieldType & Text.intercalate ", "
     in mconcat [name, ".validate(", appliedDecoders, ")"]
decoderForDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedDecoders = appliedTypes & fmap validatorForFieldType & Text.intercalate ", "
     in mconcat [moduleName, ".", name, ".validate(", appliedDecoders, ")"]
decoderForDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedDecoders = appliedTypes & fmap validatorForFieldType & Text.intercalate ", "
     in mconcat [moduleName, ".", name, ".validate(", appliedDecoders, ")"]
decoderForDefinitionReference
  (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
    mconcat [moduleName, ".", name, ".validate"]

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
                        constructorName & upperCaseFirstCharacter & sanitizeName,
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
                  & fmap
                    ( \(TypeVariable t) ->
                        mconcat
                          [ "validate_" <> t,
                            ": validation.Validator[",
                            t,
                            "]"
                          ]
                    )
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
  constructor@(Constructor (ConstructorName constructorName) maybePayload) =
    let name = sanitizeName constructorName
        payloadTypeVariables =
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
              constructorName,
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
                      constructorName,
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

sanitizeName :: Text -> Text
sanitizeName "None" = "None_"
sanitizeName name = name

outputEncoderForUnionConstructor :: FieldName -> Constructor -> Text
outputEncoderForUnionConstructor
  (FieldName tag)
  (Constructor (ConstructorName name) maybePayload) =
    let maybeDataField =
          maybe "" (dataEncoder >>> (", 'data': " <>)) maybePayload
        dataEncoder (BasicType _) = "self.data"
        dataEncoder (DefinitionReferenceType _) = "self.data.to_json()"
        dataEncoder (TypeVariableReferenceType _typeVariable) =
          "encoding.general_to_json(self.data)"
        dataEncoder fieldType = mconcat [encoderForFieldType fieldType, "(self.data)"]
        interface = mconcat ["{", mconcat ["'", tag, "': '", name, "'", maybeDataField], "}"]
     in mconcat
          [ mconcat ["    def to_json(self) -> typing.Dict[str, typing.Any]:\n"],
            mconcat ["        return ", interface, "\n\n"],
            mconcat ["    def encode(self) -> str:\n"],
            mconcat ["        return json.dumps(self.to_json())"]
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

outputField :: Int -> StructField -> Text
outputField indentation (StructField (FieldName name) fieldType) =
  let indent = Text.pack $ replicate indentation ' '
   in indent <> mconcat [name, ": ", outputFieldType fieldType]

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
outputFieldType (RecursiveReferenceType (DefinitionName name)) = mconcat ["'", name, "'"]
outputFieldType (DefinitionReferenceType definitionReference) =
  outputDefinitionReference definitionReference
outputFieldType (TypeVariableReferenceType (TypeVariable t)) = t

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
     in mconcat [name, "[", appliedFieldTypes, "]"]
outputDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate ", "
     in mconcat [moduleName, ".", name, "[", appliedFieldTypes, "]"]
outputDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate ", "
        maybeAppliedOutput = if null appliedTypes then "" else mconcat ["[", appliedFieldTypes, "]"]
     in mconcat [moduleName, ".", name, maybeAppliedOutput]
outputDefinitionReference (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
  mconcat [moduleName, ".", name]

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
fieldTypeName (BasicType BasicString) = "str"
fieldTypeName (BasicType F32) = "float"
fieldTypeName (BasicType F64) = "float"
fieldTypeName (BasicType U8) = "int"
fieldTypeName (BasicType U16) = "int"
fieldTypeName (BasicType U32) = "int"
fieldTypeName (BasicType U64) = "int"
fieldTypeName (BasicType U128) = "int"
fieldTypeName (BasicType I8) = "int"
fieldTypeName (BasicType I16) = "int"
fieldTypeName (BasicType I32) = "int"
fieldTypeName (BasicType I64) = "int"
fieldTypeName (BasicType I128) = "int"
fieldTypeName (BasicType Boolean) = "bool"
fieldTypeName (TypeVariableReferenceType (TypeVariable t)) = t
fieldTypeName (ComplexType (ArrayType _ _arrayFieldType)) = "list"
fieldTypeName (ComplexType (SliceType _sliceFieldType)) = "list"
fieldTypeName (ComplexType (PointerType pointerFieldType)) = fieldTypeName pointerFieldType
fieldTypeName (ComplexType (OptionalType optionalFieldType)) = fieldTypeName optionalFieldType
fieldTypeName
  ( DefinitionReferenceType
      (DefinitionReference (TypeDefinition (DefinitionName definitionName) _))
    ) =
    definitionName
fieldTypeName
  ( DefinitionReferenceType
      (ImportedDefinitionReference moduleName (TypeDefinition (DefinitionName definitionName) _))
    ) =
    mconcat [unModuleName moduleName, ".", definitionName]
fieldTypeName
  ( DefinitionReferenceType
      ( AppliedGenericReference
          _fieldTypes
          (TypeDefinition (DefinitionName definitionName) _)
        )
    ) =
    definitionName
fieldTypeName
  ( DefinitionReferenceType
      ( AppliedImportedGenericReference
          moduleName
          (AppliedTypes _fieldTypes)
          (TypeDefinition (DefinitionName definitionName) _)
        )
    ) =
    mconcat [unModuleName moduleName, ".", definitionName]
fieldTypeName
  ( DefinitionReferenceType
      ( GenericDeclarationReference
          moduleName
          (DefinitionName definitionName)
          (AppliedTypes _fieldTypes)
        )
    ) =
    mconcat [unModuleName moduleName, ".", definitionName]
fieldTypeName
  (DefinitionReferenceType (DeclarationReference moduleName (DefinitionName definitionName))) =
    mconcat [unModuleName moduleName, ".", definitionName]

joinTypeVariables :: [TypeVariable] -> Text
joinTypeVariables typeVariables =
  typeVariables
    & fmap (\(TypeVariable t) -> t)
    & Text.intercalate ", "
    & (\o -> "[" <> o <> "]")

nameOfReference :: DefinitionReference -> DefinitionName
nameOfReference (DefinitionReference (TypeDefinition name _)) = name
nameOfReference (ImportedDefinitionReference _moduleName (TypeDefinition name _)) = name
nameOfReference (AppliedGenericReference _fieldTypes (TypeDefinition name _)) = name
nameOfReference (AppliedImportedGenericReference _moduleName _fieldTypes (TypeDefinition name _)) =
  name
nameOfReference (GenericDeclarationReference _moduleName name _fieldTypes) = name
nameOfReference (DeclarationReference _moduleName name) = name
