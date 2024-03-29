{-# LANGUAGE AllowAmbiguousTypes #-}

module CodeGeneration.Kotlin (outputModule) where

import CodeGeneration.Utilities
import Data.Kind (Type)
import Qtility
import qualified RIO.List as List
import qualified RIO.Text as Text
import Types

-- | Type class for outputting union constructors, useful for generalizing between embedded and
-- standard constructors.
class (HasName c) => ConstructorOutput c where
  type PayloadType c :: Type
  payloadL :: Lens' c (Maybe (PayloadType c))
  dataFieldOutput :: PayloadType c -> Text

instance ConstructorOutput Constructor where
  type PayloadType Constructor = FieldType
  payloadL = constructorPayloadType
  dataFieldOutput fieldType = do
    mconcat ["(val data: ", outputFieldType fieldType, ")"]

instance ConstructorOutput EmbeddedConstructor where
  type PayloadType EmbeddedConstructor = DefinitionReference
  payloadL = embeddedConstructorPayload
  dataFieldOutput definitionReference = do
    mconcat ["(@JsonValue(true) val data: ", nameOf definitionReference, ")"]

outputModule :: Module -> Text
outputModule module' =
  let definitionOutput =
        module' ^. moduleDefinitions & mapMaybe outputDefinition & Text.intercalate "\n\n"
      importsOutput = module' ^. moduleImports & fmap outputImport & Text.intercalate "\n"
      outputImport import' =
        mconcat
          [ "import org.gotynoOutput.",
            import' & nameOf & uppercaseModuleName
          ]
      moduleClassOutput = mconcat ["class ", module' & nameOf & uppercaseModuleName, " {\n"]
   in mconcat
        [ "package org.gotynoOutput\n\n",
          modulePrelude,
          "\n\n",
          if Text.null importsOutput then "" else importsOutput <> "\n\n",
          if null (module' ^. moduleDeclarationNames)
            then ""
            else "import org.gotynoDeclarations.*" <> "\n\n",
          moduleClassOutput,
          definitionOutput,
          "\n",
          "}\n"
        ]

modulePrelude :: Text
modulePrelude =
  mconcat
    [ "import com.fasterxml.jackson.annotation.*\n",
      "import com.fasterxml.jackson.module.kotlin.*\n",
      "import com.fasterxml.jackson.databind.annotation.*\n",
      "import com.fasterxml.jackson.databind.*\n",
      "import com.fasterxml.jackson.core.*\n",
      "import com.fasterxml.jackson.databind.deser.std.*\n",
      "import java.text.ParseException\n",
      "import java.math.BigInteger\n",
      "import kotlinx.serialization.Serializable\n",
      "import org.gotynoDeclarations.BigIntegerSerializer"
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

outputUntaggedUnion :: DefinitionName -> [FieldType] -> Text
outputUntaggedUnion unionName cases =
  let typeHeaderOutput =
        mconcat ["@Serializable\n", "sealed class ", nameOf unionName, " : java.io.Serializable {"]
      jsonSerializationAnnotation =
        mconcat ["@JsonDeserialize(using = ", nameOf unionName, ".Deserializer::class)"]
      dataClassesOutput = cases & fmap outputDataClass & Text.intercalate "\n"
      caseName fieldType = "_" <> fieldTypeName fieldType
      sortStringLast = List.sortBy compareFieldTypes
      compareFieldTypes (BasicType BasicString) _other = GT
      compareFieldTypes _other (BasicType BasicString) = LT
      compareFieldTypes _other _other2 = EQ
      outputDataClass fieldType =
        mconcat
          [ "    @Serializable\n",
            "    data class ",
            caseName fieldType,
            "(@JsonValue(true) val data: ",
            outputFieldType fieldType,
            ") : ",
            nameOf unionName,
            "()"
          ]
      deserializerClassOutput =
        mconcat
          [ "    class Deserializer : StdDeserializer<",
            nameOf unionName,
            ">(",
            nameOf unionName,
            "::class.java) {\n",
            "        override fun deserialize(p: JsonParser, ctxt: DeserializationContext): ",
            nameOf unionName,
            " {\n",
            "            val text = ctxt.readTree(p).toString()\n",
            "            val mapper = jacksonObjectMapper()\n\n",
            -- @NOTE: Jackson will read any scalar value as a string because it's badly designed,
            -- so we need to read strings last.
            cases & sortStringLast & fmap outputDeserializerCase & Text.intercalate "\n",
            "\n",
            mconcat
              [ "            throw ParseException(\"Could not deserialize to class '",
                nameOf unionName,
                "'\", 0)"
              ],
            "\n",
            "        }\n",
            "    }"
          ]
      outputDeserializerCase fieldType =
        mconcat
          [ "            try { return ",
            caseName fieldType,
            "(mapper.readValue(text)) } catch (_: Exception)  {}"
          ]
   in mconcat
        [ jsonSerializationAnnotation,
          "\n",
          typeHeaderOutput,
          "\n",
          dataClassesOutput,
          "\n\n",
          deserializerClassOutput,
          "\n",
          "}"
        ]

outputEnumeration :: BasicTypeValue -> DefinitionName -> [EnumerationValue] -> Text
outputEnumeration basicType name values' =
  let valuesOutput =
        values'
          & fmap
            ( \(EnumerationValue (EnumerationIdentifier i) literal) ->
                mconcat
                  [ "    @JsonProperty(",
                    outputLiteralValue literal,
                    ") ",
                    Text.toUpper i,
                    "(",
                    outputLiteralValue literal,
                    ")"
                  ]
            )
          & Text.intercalate ",\n"
      outputLiteralValue (LiteralString t) = mconcat ["\"", t, "\""]
      outputLiteralValue (LiteralInteger i) = tshow i
      outputLiteralValue (LiteralFloat f) = tshow f
      outputLiteralValue (LiteralBoolean b) = bool "false" "true" b
   in mconcat
        [ mconcat
            [ "enum class ",
              nameOf name,
              "(val data: ",
              outputFieldType $ BasicType basicType,
              ") : java.io.Serializable {\n"
            ],
          valuesOutput,
          ";\n",
          "\n",
          "    companion object {}\n",
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
      typeOutput = mconcat ["@Serializable\n", "data class ", nameOf name]
   in mconcat
        [ typeOutput,
          "(\n    ",
          fieldsOutput,
          "\n) : java.io.Serializable {\n",
          outputStructCompanion name fields [],
          "}"
        ]

outputGenericStruct :: DefinitionName -> [TypeVariable] -> [StructField] -> Text
outputGenericStruct name typeVariables fields =
  let fieldsOutput = fields & fmap outputField & Text.intercalate ",\n    "
      typeVariablesOutput = mconcat ["<", joinTypeVariables typeVariables, ">"]
      typeOutput = mconcat ["@Serializable\n", "data class ", nameOf name, typeVariablesOutput]
   in mconcat
        [ typeOutput,
          "(\n    ",
          fieldsOutput,
          "\n",
          ") : java.io.Serializable {\n",
          outputStructCompanion name fields typeVariables,
          "}"
        ]

outputStructCompanion :: DefinitionName -> [StructField] -> [TypeVariable] -> Text
outputStructCompanion name fields typeVariables =
  let createOutput =
        mconcat
          [ "        fun ",
            typeVariableOutput <> if null typeVariables then "" else " ",
            "create",
            "(",
            createParameterOutput,
            "): ",
            nameOf name,
            typeVariableOutput,
            " {\n",
            "            return ",
            nameOf name,
            "(",
            createArgumentOutput,
            ")\n",
            "        }\n"
          ]
      (optionalFields, nonOptionalFields) = List.partition isOptionalField fields
      (literalFields, nonLiteralFields) = List.partition isLiteralField nonOptionalFields
      typeVariableOutput =
        if null typeVariables then "" else joinedTypeVariables
      joinedTypeVariables = mconcat ["<", joinTypeVariables typeVariables, ">"]
      createParameterOutput =
        (nonLiteralFields <> optionalFields <> literalFields)
          & fmap outputFieldParameter
          & Text.intercalate ", "
      outputFieldParameter f
        | isOptionalField f =
          mconcat [nameOf f, ": ", f ^. structFieldType & outputFieldType, " = null"]
        | otherwise = mconcat [nameOf f, ": ", f ^. structFieldType & outputFieldType]
      isOptionalField (StructField _ (ComplexType (OptionalType _))) = True
      isOptionalField _ = False
      isLiteralField (StructField _ (LiteralType _)) = True
      isLiteralField _ = False
      createArgumentOutput =
        fields
          & fmap outputFieldArgument
          & Text.intercalate ", "
      outputFieldArgument f = mconcat [nameOf f, " = ", nameOf f]
   in mconcat ["    companion object {\n", createOutput, "    }\n"]

outputUnion :: DefinitionName -> FieldName -> UnionType -> Text
outputUnion name typeTag unionType =
  let caseUnionOutput =
        unionType
          & constructorsFrom
          & fmap (outputUnionClass name typeTag typeVariables)
          & Text.intercalate "\n\n"
      constructorsFrom (PlainUnion constructors) = constructors
      constructorsFrom (GenericUnion _typeVariables constructors) = constructors
      typeVariables = unionType & typeVariablesFrom & fromMaybe []
   in mconcat
        [ outputUnionTypeDeclaration name typeTag typeVariables,
          " {\n",
          caseUnionOutput,
          "\n}"
        ]

outputEmbeddedUnion :: DefinitionName -> FieldName -> [EmbeddedConstructor] -> Text
outputEmbeddedUnion unionName typeTag constructors =
  mconcat
    [ outputUnionTypeDeclaration unionName typeTag [],
      " {\n",
      constructors & fmap (outputUnionClass unionName typeTag []) & Text.intercalate "\n\n",
      "\n",
      "}"
    ]

outputUnionTypeDeclaration :: DefinitionName -> FieldName -> [TypeVariable] -> Text
outputUnionTypeDeclaration name typeTag typeVariables = do
  let maybeTypeVariables =
        if null typeVariables then "" else mconcat ["<", joinTypeVariables typeVariables, ">"]
  mconcat
    [ outputUnionJsonTypeInfo typeTag,
      "\n",
      mconcat ["sealed class ", nameOf name, maybeTypeVariables, " : java.io.Serializable"]
    ]

outputUnionClass ::
  forall c.
  (ConstructorOutput c) =>
  DefinitionName ->
  FieldName ->
  [TypeVariable] ->
  c ->
  Text
outputUnionClass unionName typeTag typeVariables constructor = do
  let typeVariablesOutput =
        if null typeVariables then "" else mconcat ["<", joinTypeVariables typeVariables, ">"]
      typeOfClass = maybe "class" (const "data class") (constructor ^. payloadL)
      constructorInfo =
        mconcat ["    @Serializable\n", "    @JsonTypeName(\"", nameOf constructor, "\")\n"]
      className = constructor & nameOf & upperCaseFirst
      classOutput =
        mconcat
          [ "    ",
            typeOfClass,
            " ",
            className,
            typeVariablesOutput,
            maybe "" (dataFieldOutput @c) (constructor ^. payloadL)
          ]
      classOverrides =
        maybe
          ( mconcat
              [ "\n",
                "        override fun equals(other: Any?): Boolean {\n",
                "            return other is ",
                className,
                genericPlaceholders,
                "\n",
                "        }\n",
                "\n",
                "        override fun hashCode(): Int {\n",
                "            return 0\n",
                "        }\n"
              ]
          )
          (const "")
          (constructor ^. payloadL)
      genericPlaceholders =
        if null typeVariables
          then ""
          else
            mconcat
              [ "<",
                typeVariables
                  & fmap ((^. unwrap) >>> const "*")
                  & Text.intercalate ",",
                ">"
              ]
      maybeUnionTypeVariableOutput =
        if null typeVariables then "" else mconcat ["<", joinTypeVariables typeVariables, ">"]
   in mconcat
        [ constructorInfo,
          classOutput,
          " : ",
          nameOf unionName,
          maybeUnionTypeVariableOutput,
          "(), java.io.Serializable {\n",
          mconcat ["        val ", nameOf typeTag, " = \"", nameOf constructor, "\"\n"],
          classOverrides,
          "    }"
        ]

outputUnionJsonTypeInfo :: FieldName -> Text
outputUnionJsonTypeInfo typeTag =
  mconcat
    [ "@Serializable\n",
      "@JsonTypeInfo(\n",
      "    use = JsonTypeInfo.Id.NAME,\n",
      "    include = JsonTypeInfo.As.EXISTING_PROPERTY,\n",
      "    property = \"",
      unFieldName typeTag,
      "\"\n",
      ")"
    ]

outputField :: StructField -> Text
outputField (StructField fieldName fieldType) =
  mconcat
    [ "@get:JsonProperty(\"",
      unFieldName fieldName,
      "\")\n",
      if isBigIntType fieldType
        then "    @Serializable(with = BigIntegerSerializer::class)\n"
        else "",
      "    val ",
      unFieldName fieldName,
      ": ",
      outputFieldType fieldType
    ]

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
  mconcat ["ArrayList<", outputFieldType fieldType, ">"]
outputFieldType (ComplexType (SliceType fieldType)) =
  mconcat ["ArrayList<", outputFieldType fieldType, ">"]
outputFieldType (ComplexType (PointerType fieldType)) = outputFieldType fieldType
outputFieldType (RecursiveReferenceType (DefinitionName name)) = name
outputFieldType (DefinitionReferenceType definitionReference) =
  outputDefinitionReference definitionReference
outputFieldType (TypeVariableReferenceType t) = unTypeVariable t

outputDefinitionReference :: DefinitionReference -> Text
outputDefinitionReference r@(DefinitionReference _typeDefinition) = nameOf r
outputDefinitionReference (ImportedDefinitionReference moduleName' typeDefinition) =
  mconcat [moduleName' & nameOf & uppercaseModuleName, ".", nameOf typeDefinition]
outputDefinitionReference (AppliedGenericReference appliedTypes typeDefinition) =
  let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate ", "
   in mconcat [nameOf typeDefinition, "<", appliedFieldTypes, ">"]
outputDefinitionReference
  (AppliedImportedGenericReference moduleName' (AppliedTypes appliedTypes) typeDefinition) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate ", "
     in mconcat
          [ moduleName' & nameOf & uppercaseModuleName,
            ".",
            nameOf typeDefinition,
            "<",
            appliedFieldTypes,
            ">"
          ]
outputDefinitionReference
  (GenericDeclarationReference moduleName' definitionName (AppliedTypes appliedTypes)) =
    let appliedFieldTypes = appliedTypes & fmap outputFieldType & Text.intercalate ", "
        maybeAppliedOutput = if null appliedTypes then "" else mconcat ["<", appliedFieldTypes, ">"]
     in mconcat
          [ moduleName'
              & nameOf
              & uppercaseModuleName,
            "_",
            nameOf definitionName,
            maybeAppliedOutput
          ]
outputDefinitionReference (DeclarationReference moduleName' definitionName') =
  mconcat [moduleName' & nameOf & uppercaseModuleName, "_", nameOf definitionName']

outputBasicType :: BasicTypeValue -> Text
outputBasicType BasicString = "String"
outputBasicType U8 = "Byte"
outputBasicType U16 = "Short"
outputBasicType U32 = "Int"
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
fieldTypeName (BasicType F32) = "Float"
fieldTypeName (BasicType F64) = "Double"
fieldTypeName (BasicType U8) = "Byte"
fieldTypeName (BasicType U16) = "Short"
fieldTypeName (BasicType U32) = "Int"
fieldTypeName (BasicType U64) = "BigInteger"
fieldTypeName (BasicType U128) = "BigInteger"
fieldTypeName (BasicType I8) = "Byte"
fieldTypeName (BasicType I16) = "Short"
fieldTypeName (BasicType I32) = "Int"
fieldTypeName (BasicType I64) = "BigInteger"
fieldTypeName (BasicType I128) = "BigInteger"
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

isBigIntType :: FieldType -> Bool
isBigIntType (ComplexType (SliceType fieldType)) = isBigIntType fieldType
isBigIntType (ComplexType (ArrayType _size fieldType)) = isBigIntType fieldType
isBigIntType (ComplexType (OptionalType fieldType)) = isBigIntType fieldType
isBigIntType (ComplexType (PointerType fieldType)) = isBigIntType fieldType
isBigIntType (BasicType U64) = True
isBigIntType (BasicType U128) = True
isBigIntType (BasicType I64) = True
isBigIntType (BasicType I128) = True
isBigIntType _ = False

joinTypeVariables :: [TypeVariable] -> Text
joinTypeVariables = fmap unTypeVariable >>> Text.intercalate ", "

uppercaseModuleName :: Text -> Text
uppercaseModuleName = upperCaseFirst
