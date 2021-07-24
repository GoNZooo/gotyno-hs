module CodeGeneration.FSharp (outputModule) where

import CodeGeneration.Utilities (upperCaseFirstCharacter)
import RIO
import qualified RIO.List.Partial as PartialList
import qualified RIO.Text as Text
import Types

outputModule :: Module -> Text
outputModule Module {name = ModuleName name, definitions, imports} =
  let definitionOutput = definitions & mapMaybe outputDefinition & Text.intercalate "\n\n"
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
                mconcat
                  [ "                \"",
                    name,
                    "\", ",
                    unionName,
                    ".",
                    upperCaseFirstCharacter name,
                    "Decoder\n"
                  ]
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
                      [ mconcat ["        | ", upperCaseFirstCharacter name, " payload ->\n"],
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
    [ upperCaseFirstCharacter fieldName,
      " = ",
      "get.Optional.Field",
      " \"",
      fieldName,
      "\" ",
      decoderForFieldType fieldType
    ]
outputDecoderForField (StructField (FieldName fieldName) fieldType) =
  mconcat
    [ upperCaseFirstCharacter fieldName,
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
      upperCaseFirstCharacter fieldName
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
decoderForBasicType U8 = "Decode.byte"
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
decoderForBasicType Boolean = "Decode.bool"

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
decoderForDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedDecoders = appliedTypes & fmap decoderForFieldType & Text.intercalate " "
     in mconcat ["(", fsharpifyModuleName moduleName, ".", name, ".Decoder ", appliedDecoders, ")"]
decoderForDefinitionReference
  (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
    mconcat [fsharpifyModuleName moduleName, ".", name, ".Decoder"]

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
encoderForBasicType U8 = "Encode.byte"
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
encoderForBasicType Boolean = "Encode.bool"

encoderForLiteralType :: LiteralTypeValue -> Text
encoderForLiteralType (LiteralString s) = "Encode.string \"" <> s <> "\""
encoderForLiteralType (LiteralInteger i) = "Encode.int32 " <> tshow i
encoderForLiteralType (LiteralFloat f) = "Encode.float32 " <> tshow f
encoderForLiteralType (LiteralBoolean b) =
  "Encode.bool " <> bool "false" "true" b

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
encoderForDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedEncoders = appliedTypes & fmap (encoderForFieldType ("", "")) & Text.intercalate " "
     in mconcat ["(", fsharpifyModuleName moduleName, ".", name, ".Encoder ", appliedEncoders, ")"]
encoderForDefinitionReference
  (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
    mconcat [fsharpifyModuleName moduleName, ".", name, ".Encoder"]

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
                        upperCaseFirstCharacter name,
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
      constructorName = upperCaseFirstCharacter name
      alwaysSucceedingDecoder = mconcat ["Decode.succeed ", constructorName]
      payloadTypeVariables = fromMaybe [] $ foldMap typeVariablesFrom maybePayload
      maybeArguments = typeVariableDecodersAsArguments payloadTypeVariables
      fullName =
        if null typeVariables
          then unionName
          else mconcat [unionName, joinTypeVariables typeVariables]
      decoderWithDataField payload =
        mconcat
          [ "Decode.object (fun get -> ",
            constructorName,
            "(get.Required.Field \"data\" ",
            decoderForFieldType payload,
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

outputField :: Int -> StructField -> Text
outputField indentation (StructField (FieldName name) fieldType) =
  let indent = Text.pack $ replicate indentation ' '
   in indent <> mconcat [upperCaseFirstCharacter name, ": ", outputFieldType fieldType, "\n"]

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

joinTypeVariables :: [TypeVariable] -> Text
joinTypeVariables typeVariables =
  typeVariables
    & fmap (\(TypeVariable t) -> fsharpifyTypeVariable t)
    & Text.intercalate ", "
    & (\o -> "<" <> o <> ">")
