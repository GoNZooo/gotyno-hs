module CodeGeneration.DLang (outputModule) where

import CodeGeneration.Utilities (HasName, MaybeHasTypeVariables (..), nameOf)
import Qtility
import qualified RIO.Char as Char
import qualified RIO.Text as Text
import Types

outputModule :: Module -> Text
outputModule module' =
  let definitionOutput =
        module' ^. moduleDefinitions & mapMaybe outputDefinition & Text.intercalate "\n\n"
      importsOutput = module' ^. moduleImports & fmap outputImport & Text.intercalate "\n"
      outputImport import' =
        let importName = import' ^. unwrap . moduleName . unwrap
         in mconcat ["static import gotyno_output.", pascalToSnake importName, ";"]
      declarationImportsOutput =
        module' ^. moduleDeclarationNames
          & fmap
            ( \(ModuleName declarationModuleName) ->
                mconcat
                  [ "import qualified GotynoDeclarations.",
                    upperCaseFirst declarationModuleName,
                    " as ",
                    upperCaseFirst declarationModuleName
                  ]
            )
          & Text.intercalate "\n"
   in mconcat
        [ module' ^. moduleName . unwrap & upperCaseFirst & modulePrelude,
          "\n",
          if Text.null importsOutput then "" else importsOutput <> "\n\n",
          if Text.null declarationImportsOutput then "" else declarationImportsOutput <> "\n\n",
          definitionOutput,
          "\n"
        ]

modulePrelude :: Text -> Text
modulePrelude name =
  Text.unlines
    [ mconcat ["module gotyno_output.", pascalToSnake name, ";"],
      "",
      "import asdf;",
      "import std.sumtype;",
      "import std.typecons : Nullable;"
    ]

outputDefinition :: TypeDefinition -> Maybe Text
outputDefinition (TypeDefinition name (Struct (PlainStruct fields))) =
  pure $ outputPlainStruct name fields
outputDefinition (TypeDefinition name (Struct (GenericStruct typeVariables fields))) =
  pure $ outputGenericStruct name typeVariables fields
outputDefinition (TypeDefinition name (Union typeTag unionType)) =
  pure $ outputUnion name typeTag unionType
outputDefinition (TypeDefinition name (Enumeration type' enumerationValues)) =
  pure $ outputEnumeration type' name enumerationValues
outputDefinition (TypeDefinition name (UntaggedUnion unionCases)) =
  pure $ outputUntaggedUnion name unionCases
outputDefinition (TypeDefinition name (EmbeddedUnion typeTag constructors)) =
  pure $ outputEmbeddedUnion name typeTag constructors
outputDefinition (TypeDefinition _name (DeclaredType _moduleName' _typeVariables)) = Nothing

outputEmbeddedUnion :: DefinitionName -> FieldName -> [EmbeddedConstructor] -> Text
outputEmbeddedUnion unionName tag constructors =
  let payloadStructsOutput =
        constructors
          & filter ((^. embeddedConstructorPayload) >>> isNothing)
          & fmap outputConstructorStruct
          & Text.intercalate "\n\n"
      outputConstructorStruct c =
        mconcat
          [ "struct ",
            c & nameOf & upperCaseFirst & ("_" <>),
            "\n",
            "{\n",
            "}"
          ]
      unionTypeOutput = outputEmbeddedUnionType tag unionName [] constructors
   in mconcat
        [ payloadStructsOutput,
          "\n\n",
          unionTypeOutput
        ]

outputUntaggedUnion :: DefinitionName -> [FieldType] -> Text
outputUntaggedUnion unionName cases =
  let typeOutput = mconcat ["data ", unDefinitionName unionName, "\n  = ", unionOutput]
      unionOutput = cases & fmap outputCaseLine & Text.intercalate "\n  | "
      caseConstructor fieldType = mconcat [nameOf unionName, fieldTypeName fieldType]
      outputCaseLine fieldType =
        mconcat
          [ caseConstructor fieldType,
            " ",
            outputFieldType fieldType
          ]
      jsonInstanceOutput =
        mconcat [fromJsonInstance, "\n\n", toJsonInstance]
      fromJsonInstance =
        mconcat
          [ "instance FromJSON ",
            unionName & nameOf & sanitizeName,
            " where\n",
            "  parseJSON v =\n",
            "    ",
            fromJsonCaseOutput
          ]
      toJsonInstance =
        mconcat
          [ "instance ToJSON ",
            unionName & nameOf & sanitizeName,
            " where\n",
            toJsonCaseOutput
          ]
      fromJsonCaseOutput =
        cases & fmap fromJsonCase & Text.intercalate "\n      <|> "
      toJsonCaseOutput =
        cases & fmap toJsonCase & Text.intercalate "\n"
      fromJsonCase fieldType = mconcat ["(", caseConstructor fieldType, " <$> parseJSON v)"]
      toJsonCase fieldType = mconcat ["  toJSON (", caseConstructor fieldType, " v) = toJSON v"]
   in mconcat
        [ typeOutput,
          "\n",
          "  deriving (Eq, Show, Generic)",
          "\n\n",
          jsonInstanceOutput
        ]

outputEnumeration :: BasicTypeValue -> DefinitionName -> [EnumerationValue] -> Text
outputEnumeration type' name values' =
  let typeHeader = mconcat ["enum ", name & nameOf & sanitizeName, " : ", outputBasicType type']
      valuesOutput =
        values'
          & fmap
            ( \(EnumerationValue (EnumerationIdentifier i) literal) ->
                mconcat
                  [ i,
                    " = ",
                    literal & LiteralType & fieldTypeName
                  ]
            )
          & Text.intercalate ",\n    "
   in mconcat [typeHeader, "\n{\n    ", valuesOutput, "\n}"]

outputPlainStruct :: DefinitionName -> [StructField] -> Text
outputPlainStruct name fields =
  let fieldsOutput = fields & fmap outputField & Text.intercalate "\n    "
   in mconcat
        [ mconcat ["struct ", unDefinitionName name, "\n"],
          "{\n    ",
          fieldsOutput,
          "\n}"
        ]

outputGenericStruct :: DefinitionName -> [TypeVariable] -> [StructField] -> Text
outputGenericStruct name typeVariables fields =
  let fullName = fullNameWithTypeVariables typeVariables name
      fieldsOutput = fields & fmap outputField & Text.intercalate ",\n    "
   in mconcat
        [ mconcat ["struct ", fullName],
          "\n",
          "{",
          "\n    ",
          fieldsOutput,
          "\n}"
        ]

outputUnion :: DefinitionName -> FieldName -> UnionType -> Text
outputUnion name _typeTag unionType =
  let payloadStructsOutput = outputCaseUnion (constructorsFrom unionType) typeVariables
      unionTypeOutput = outputUnionType name typeVariables (constructorsFrom unionType)
      constructorsFrom (PlainUnion constructors) = constructors
      constructorsFrom (GenericUnion _typeVariables constructors) = constructors
      typeVariables = case unionType of
        PlainUnion _constructors -> []
        GenericUnion ts _constructors -> ts
   in mconcat
        [ payloadStructsOutput,
          "\n\n",
          unionTypeOutput
        ]

outputUnionType :: DefinitionName -> [TypeVariable] -> [Constructor] -> Text
outputUnionType name typeVariables constructors =
  let fullName = mconcat [nameOf name, typeVariableOutput]
      typeVariableOutput =
        if null typeVariables then "" else mconcat ["(", joinTypeVariables typeVariables, ")"]
      joinedPayloadNames = constructors & fmap appliedConstructorName & Text.intercalate ", "
      appliedConstructorName c =
        c
          & constructorName . unwrap %~ ("_" <>)
          & appliedNameWithTypeVariables (c & typeVariablesFrom & fromMaybe [])
      thisConstructorOutput =
        Text.unlines
          [ "    static foreach (T; Type.Types)",
            "        this(T v) @safe pure nothrow @nogc { data = v; }"
          ]
      outputConstructorDeserializerCase c@Constructor {_constructorPayloadType = Nothing} =
        mconcat
          [ "            case \"",
            c & nameOf & sanitizeName,
            "\": {\n",
            "                data = ",
            fullNameWithTypeVariables
              (c & typeVariablesFrom & fromMaybe [])
              (c & constructorName . unwrap %~ ("_" <>)),
            "();\n",
            "                return null;\n",
            "            }"
          ]
      outputConstructorDeserializerCase c@Constructor {_constructorPayloadType = Just p} =
        mconcat
          [ "            case \"",
            nameOf c,
            "\": {\n",
            "                ",
            appliedNameWithTypeVariables
              (p & typeVariablesFrom & fromMaybe [])
              (c & constructorName . unwrap %~ ("_" <>)),
            " v = void;\n",
            "                if (auto e = asdfData.deserializeValue(v)) return e;\n",
            "                data = v;\n",
            "                return null;\n",
            "            }"
          ]
      deserializerCases =
        constructors
          & fmap outputConstructorDeserializerCase
          & Text.intercalate "\n\n"
      deserializerOutput =
        mconcat
          [ "    import asdf;\n",
            "    SerdeException deserializeFromAsdf(Asdf asdfData)\n",
            "    {\n",
            "        string tag;\n",
            "        if (auto e = asdfData[\"type\"].deserializeValue(tag)) return e;\n",
            "\n",
            "        final switch (tag)\n",
            "        {\n",
            deserializerCases <> "\n\n",
            "            default: return new SerdeException(\"Unknown tag: \" ~ tag);\n",
            "        }\n",
            "    }"
          ]
   in mconcat
        [ mconcat ["struct ", fullName, "\n{\n"],
          mconcat ["    alias Type = SumType!(", joinedPayloadNames, ");\n"],
          "    Type data;\n",
          "    alias data this;\n",
          "\n",
          thisConstructorOutput,
          "\n",
          deserializerOutput,
          "\n",
          "}"
        ]

outputCaseUnion :: [Constructor] -> [TypeVariable] -> Text
outputCaseUnion constructors _typeVariables =
  constructors & fmap outputCaseConstructor & Text.intercalate "\n\n"
  where
    outputCaseConstructor (Constructor (ConstructorName constructorName') Nothing) =
      let sanitizedName = constructorName' & upperCaseFirst & sanitizeName
       in mconcat ["struct _", sanitizedName, "\n{\n}"]
    outputCaseConstructor c@(Constructor _constructorName (Just payload)) =
      let payloadOutput = outputFieldType payload
          fullName =
            c
              & constructorName . unwrap %~ (upperCaseFirst >>> sanitizeName >>> ("_" <>))
              & fullNameWithTypeVariables (payload & typeVariablesFrom & fromMaybe [])
       in mconcat
            [ "struct ",
              fullName,
              "\n{\n    ",
              payloadOutput,
              " data;\n}"
            ]

outputEmbeddedUnionType ::
  FieldName ->
  DefinitionName ->
  [TypeVariable] ->
  [EmbeddedConstructor] ->
  Text
outputEmbeddedUnionType tag name typeVariables constructors =
  let fullName = mconcat [nameOf name, typeVariableOutput]
      typeVariableOutput =
        if null typeVariables then "" else mconcat ["(", joinTypeVariables typeVariables, ")"]
      joinedPayloadNames = constructors & fmap appliedConstructorName & Text.intercalate ", "
      appliedConstructorName c =
        c
          & embeddedConstructorName . unwrap %~ ("_" <>)
          & appliedNameWithTypeVariables []
      thisConstructorOutput =
        Text.unlines
          [ "    static foreach (T; Type.Types)",
            "        this(T v) @safe pure nothrow @nogc { data = v; }"
          ]
      outputConstructorDeserializerCase c@EmbeddedConstructor {_embeddedConstructorPayload = Nothing} =
        mconcat
          [ "            case \"",
            c & nameOf & sanitizeName,
            "\": {\n",
            "                data = ",
            fullNameWithTypeVariables
              []
              (c & embeddedConstructorName . unwrap %~ ("_" <>)),
            "();\n",
            "                return null;\n",
            "            }"
          ]
      outputConstructorDeserializerCase c@EmbeddedConstructor {_embeddedConstructorPayload = Just p} =
        mconcat
          [ "            case \"",
            nameOf c,
            "\": {\n",
            "                ",
            appliedNameWithTypeVariables (p & typeVariablesFrom & fromMaybe []) p,
            " v = void;\n",
            "                if (auto e = asdfData.deserializeValue(v)) return e;\n",
            "                data = v;\n",
            "                return null;\n",
            "            }"
          ]
      deserializerCases =
        constructors
          & fmap outputConstructorDeserializerCase
          & Text.intercalate "\n\n"
      deserializerOutput =
        mconcat
          [ "    import asdf;\n",
            "    SerdeException deserializeFromAsdf(Asdf asdfData)\n",
            "    {\n",
            "        string tag;\n",
            mconcat
              [ "        if (auto e = asdfData[\"",
                tag ^. unwrap,
                "\"].deserializeValue(tag)) return e;\n"
              ],
            "\n",
            "        final switch (tag)\n",
            "        {\n",
            deserializerCases <> "\n\n",
            "            default: return new SerdeException(\"Unknown tag: \" ~ tag);\n",
            "        }\n",
            "    }"
          ]
   in mconcat
        [ mconcat ["struct ", fullName, "\n{\n"],
          mconcat ["    alias Type = SumType!(", joinedPayloadNames, ");\n"],
          "    Type data;\n",
          "    alias data this;\n",
          "\n",
          thisConstructorOutput,
          "\n",
          deserializerOutput,
          "\n",
          "}"
        ]

sanitizeName :: (Eq s, IsString s) => s -> s
sanitizeName "private" = "_private"
sanitizeName "protected" = "_protected"
sanitizeName "public" = "_public"
sanitizeName other = other

outputField :: StructField -> Text
outputField (StructField fieldName fieldType) =
  mconcat [outputFieldType fieldType, " ", fieldName & nameOf & sanitizeName, ";"]

outputFieldType :: FieldType -> Text
outputFieldType (LiteralType (LiteralString _text)) = outputBasicType BasicString
outputFieldType (LiteralType (LiteralInteger _x)) = outputBasicType I32
outputFieldType (LiteralType (LiteralFloat _f)) = outputBasicType F32
outputFieldType (LiteralType (LiteralBoolean _b)) = outputBasicType Boolean
outputFieldType (BasicType basicType) = outputBasicType basicType
outputFieldType (ComplexType (OptionalType fieldType)) =
  mconcat ["Nullable!(", outputFieldType fieldType, ")"]
outputFieldType (ComplexType (ArrayType size fieldType)) =
  mconcat [outputFieldType fieldType, "[", tshow size, "]"]
outputFieldType (ComplexType (SliceType fieldType)) =
  mconcat [outputFieldType fieldType, "[]"]
outputFieldType (ComplexType (PointerType fieldType)) = outputFieldType fieldType
outputFieldType (RecursiveReferenceType (DefinitionName name)) = name
outputFieldType (DefinitionReferenceType definitionReference) =
  outputDefinitionReference definitionReference
outputFieldType (TypeVariableReferenceType (TypeVariable t)) = t

outputDefinitionReference :: DefinitionReference -> Text
outputDefinitionReference (DefinitionReference (TypeDefinition (DefinitionName name) _)) = name
outputDefinitionReference
  ( ImportedDefinitionReference
      (ModuleName moduleName')
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    mconcat [pascalToSnake moduleName', ".", sanitizeName name]
outputDefinitionReference
  ( AppliedGenericReference
      appliedTypes
      (TypeDefinition (DefinitionName name) _)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate " "
     in mconcat [name, "!(", appliedFieldTypes, ")"]
outputDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName')
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate ", "
     in mconcat [pascalToSnake moduleName', ".", sanitizeName name, "!(", appliedFieldTypes, ")"]
outputDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName')
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate " "
        maybeAppliedOutput = if null appliedTypes then "" else mconcat [" ", appliedFieldTypes]
     in mconcat ["(", upperCaseFirst moduleName', ".", sanitizeName name, maybeAppliedOutput, ")"]
outputDefinitionReference (DeclarationReference (ModuleName moduleName') (DefinitionName name)) =
  mconcat [pascalToSnake moduleName', ".", sanitizeName name]

outputBasicType :: BasicTypeValue -> Text
outputBasicType BasicString = "string"
outputBasicType U8 = "uint8_t"
outputBasicType U16 = "uint16_t"
outputBasicType U32 = "uint32_t"
outputBasicType U64 = "uint64_t"
outputBasicType U128 = "BigInt"
outputBasicType I8 = "int8_t"
outputBasicType I16 = "int16_t"
outputBasicType I32 = "int32_t"
outputBasicType I64 = "int64_t"
outputBasicType I128 = "BigInt"
outputBasicType F32 = "float"
outputBasicType F64 = "double"
outputBasicType Boolean = "bool"

fieldTypeName :: FieldType -> Text
fieldTypeName (LiteralType (LiteralBoolean b)) = b & tshow & lowerCaseFirst
fieldTypeName (LiteralType (LiteralString s)) = tshow s
fieldTypeName (LiteralType (LiteralInteger i)) = tshow i
fieldTypeName (LiteralType (LiteralFloat f)) = tshow f
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
          _moduleName'
          (AppliedTypes fieldTypes)
          (TypeDefinition (DefinitionName definitionName) _)
        )
    ) =
    mconcat [definitionName, "Of", fieldTypes & fmap fieldTypeName & mconcat]
fieldTypeName
  ( DefinitionReferenceType
      ( GenericDeclarationReference
          (ModuleName _moduleName')
          (DefinitionName definitionName)
          (AppliedTypes fieldTypes)
        )
    ) =
    mconcat [definitionName, "Of", fieldTypes & fmap fieldTypeName & mconcat]
fieldTypeName
  (DefinitionReferenceType (DeclarationReference _moduleName' (DefinitionName definitionName))) =
    definitionName

joinTypeVariables :: [TypeVariable] -> Text
joinTypeVariables =
  fmap (\(TypeVariable t) -> t) >>> Text.intercalate ", "

pascalToSnake :: Text -> Text
pascalToSnake =
  lowerCaseFirst
    >>> Text.foldr
      ( \c t ->
          if Char.isUpper c
            then '_' `Text.cons` Char.toLower c `Text.cons` t
            else c `Text.cons` t
      )
      ""

fullNameWithTypeVariables :: HasName a => [TypeVariable] -> a -> Text
fullNameWithTypeVariables [] a = nameOf a
fullNameWithTypeVariables typeVariables a =
  mconcat [nameOf a, "(", joinTypeVariables typeVariables, ")"]

appliedNameWithTypeVariables :: HasName a => [TypeVariable] -> a -> Text
appliedNameWithTypeVariables [] a = nameOf a
appliedNameWithTypeVariables typeVariables a =
  mconcat [nameOf a, "!(", joinTypeVariables typeVariables, ")"]
