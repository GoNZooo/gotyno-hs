module CodeGeneration.DLang (outputModule) where

import CodeGeneration.Utilities (nameOf)
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
         in mconcat
              [ "import qualified GotynoOutput.",
                upperCaseFirst importName,
                " as ",
                upperCaseFirst importName
              ]
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
      "import std.sumtype;"
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
outputDefinition (TypeDefinition _name (DeclaredType _moduleName' _typeVariables)) = Nothing

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
            unionName & nameOf & sanitizeName,
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
            upperCaseFirst n,
            " payload) = toJSON payload & atKey \"",
            tag,
            "\" ?~ String \"",
            n,
            "\""
          ]
      constructorToJson (EmbeddedConstructor (ConstructorName n) Nothing) =
        mconcat
          [ "toJSON ",
            upperCaseFirst n,
            " = object [] & atKey \"",
            tag,
            "\" ?~ String \"",
            n,
            "\""
          ]
      fromJsonOutput =
        mconcat
          [ "instance FromJSON ",
            unionName & nameOf & sanitizeName,
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
        mconcat ["\"", n, "\" -> ", upperCaseFirst n, " <$> parseJSON (Object o)"]
      constructorFromJson (EmbeddedConstructor (ConstructorName n) Nothing) =
        mconcat ["\"", n, "\" -> pure ", upperCaseFirst n]
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

outputEnumeration :: DefinitionName -> [EnumerationValue] -> Text
outputEnumeration name values' =
  let typeOutput = outputEnumerationType name values'
      toJsonOutput =
        mconcat
          [ "instance ToJSON ",
            unDefinitionName name,
            " where\n",
            constructorToJsonOutput
          ]
      constructorToJsonOutput =
        values'
          & fmap
            ( \(EnumerationValue (EnumerationIdentifier i) literal) ->
                let n =
                      mconcat
                        [ haskellifyConstructorName $ unDefinitionName name,
                          haskellifyConstructorName i
                        ]
                 in mconcat
                      [ "  toJSON ",
                        n,
                        " = ",
                        outputLiteral literal
                      ]
            )
          & Text.intercalate "\n"
      outputLiteral (LiteralString s) = mconcat ["String \"", s, "\""]
      outputLiteral (LiteralInteger i) = mconcat ["Number $ fromInteger", tshow i]
      outputLiteral (LiteralFloat f) = mconcat ["Number ", tshow f]
      outputLiteral (LiteralBoolean b) = mconcat ["Boolean ", tshow b]
      fromJsonOutput =
        mconcat
          [ "instance FromJSON ",
            unDefinitionName name,
            " where\n  parseJSON = Helpers.enumFromJSON [",
            constructorFromJsonOutput,
            "]"
          ]
      constructorFromJsonOutput =
        values'
          & fmap
            ( \(EnumerationValue (EnumerationIdentifier i) literal) ->
                let n =
                      mconcat
                        [ haskellifyConstructorName $ unDefinitionName name,
                          haskellifyConstructorName i
                        ]
                 in mconcat ["(", outputLiteral literal, ", ", n, ")"]
            )
          & Text.intercalate ", "
   in mconcat [typeOutput, "\n\n", toJsonOutput, "\n\n", fromJsonOutput]

outputEnumerationType :: DefinitionName -> [EnumerationValue] -> Text
outputEnumerationType name values' =
  let valuesOutput =
        values'
          & fmap
            ( \(EnumerationValue (EnumerationIdentifier i) _literal) ->
                mconcat
                  [ haskellifyConstructorName $ unDefinitionName name,
                    haskellifyConstructorName i
                  ]
            )
          & Text.intercalate "\n  | "
   in mconcat
        [ mconcat ["data ", unDefinitionName name, "\n  = "],
          valuesOutput,
          "\n  deriving (Eq, Show, Generic)"
        ]

haskellifyConstructorName :: Text -> Text
haskellifyConstructorName = upperCaseFirst

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
  let fullName = mconcat [nameOf name, "(", joinTypeVariables typeVariables, ")"]
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
  let payloadStructsOutput = outputCaseUnion name (constructorsFrom unionType) typeVariables
      unionTypeOutput = mconcat ["alias ", nameOf name, " = ", "SumType!(", payloadNames, ");"]
      payloadNames =
        unionType & constructorsFrom & fmap (nameOf >>> (nameOf name <>)) & Text.intercalate ", "
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

outputCaseUnion :: DefinitionName -> [Constructor] -> [TypeVariable] -> Text
outputCaseUnion name constructors _typeVariables =
  constructors & fmap outputCaseConstructor & Text.intercalate "\n\n"
  where
    outputCaseConstructor (Constructor (ConstructorName constructorName') Nothing) =
      let sanitizedName = constructorName' & upperCaseFirst & sanitizeName
       in mconcat ["struct ", nameOf name, sanitizedName, "\n{\n}"]
    outputCaseConstructor (Constructor (ConstructorName constructorName') (Just payload)) =
      let sanitizedName = constructorName' & upperCaseFirst & sanitizeName
          payloadOutput = outputFieldType payload
       in mconcat ["struct ", nameOf name, sanitizedName, "\n{\n    ", payloadOutput, " data;\n}"]

sanitizeName :: s -> s
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
  mconcat ["(Maybe ", outputFieldType fieldType, ")"]
outputFieldType (ComplexType (ArrayType _size fieldType)) =
  mconcat ["[", outputFieldType fieldType, "]"]
outputFieldType (ComplexType (SliceType fieldType)) =
  mconcat ["[", outputFieldType fieldType, "]"]
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
    mconcat [upperCaseFirst moduleName', ".", sanitizeName name]
outputDefinitionReference
  ( AppliedGenericReference
      appliedTypes
      (TypeDefinition (DefinitionName name) _)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate " "
     in mconcat ["(", name, " ", appliedFieldTypes, ")"]
outputDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName')
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _)
    ) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate " "
     in mconcat
          [ "(",
            upperCaseFirst moduleName',
            ".",
            sanitizeName name,
            " ",
            appliedFieldTypes,
            ")"
          ]
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
  mconcat [upperCaseFirst moduleName', ".", sanitizeName name]

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
