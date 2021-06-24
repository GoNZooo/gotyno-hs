module CodeGeneration.TypeScript where

import RIO
import qualified RIO.Text as Text
import Types

outputModule :: Module -> Text
outputModule Module {definitions, imports} =
  let definitionOutput = definitions & fmap outputDefinition & Text.intercalate "\n\n"
      importsOutput = imports & fmap outputImport & mconcat
      outputImport (Import Module {name = ModuleName name}) =
        mconcat ["import * as ", name, " from \"./", name, "\";\n\n"]
   in mconcat [modulePrelude, "\n\n", importsOutput, definitionOutput]

modulePrelude :: Text
modulePrelude = "import * as svt from \"simple-validation-tools\";"

outputDefinition :: TypeDefinition -> Text
outputDefinition (TypeDefinition (DefinitionName name) (Struct (PlainStruct fields))) =
  outputPlainStruct name fields
outputDefinition (TypeDefinition (DefinitionName _name) (Struct (GenericStruct _typeVariables _fields))) = ""
outputDefinition (TypeDefinition (DefinitionName name) (Union tagType unionType)) =
  outputUnion name tagType unionType
outputDefinition (TypeDefinition (DefinitionName name) (Enumeration enumerationValues)) =
  outputEnumeration name enumerationValues
outputDefinition (TypeDefinition (DefinitionName _name) (UntaggedUnion _unionCases)) = ""

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
  let export = mconcat ["export type ", name, " = {\n"]
      fieldsOutput = fields & fmap outputField & mconcat
      typeGuardOutput = outputStructTypeGuard name fields
      validatorOutput = outputStructValidator name fields
   in mconcat [export, fieldsOutput, "};\n\n", typeGuardOutput, "\n\n", validatorOutput]

outputStructValidator :: Text -> [StructField] -> Text
outputStructValidator name fields =
  let validatorPrelude =
        mconcat
          [ mconcat ["export function validate", name, "(value: unknown): svt.ValidationResult<", name, "> {\n"],
            mconcat ["    return svt.validate<", name, ">(value, {"]
          ]
      fieldsOutput = fields & fmap outputValidatorForField & Text.intercalate ", "
   in mconcat [validatorPrelude, fieldsOutput, "});\n}"]

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
      (DefinitionName name)
      _typeData
    ) =
    mconcat [moduleName, ".validate", name]
outputValidatorForDefinitionReference _ = ""

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

outputStructTypeGuard :: Text -> [StructField] -> Text
outputStructTypeGuard name fields =
  let typeGuardPrelude =
        mconcat
          [ mconcat ["export function is", name, "(value: unknown): value is ", name, " {\n"],
            mconcat ["    return svt.isInterface<", name, ">(value, {"]
          ]
      fieldsOutput = fields & fmap outputStructTypeGuardForField & Text.intercalate ", "
   in mconcat [typeGuardPrelude, fieldsOutput, "});\n}"]

outputStructTypeGuardForField :: StructField -> Text
outputStructTypeGuardForField (StructField (FieldName fieldName) fieldType) =
  mconcat [fieldName, ": ", outputTypeGuardForFieldType fieldType]

outputTypeGuardForFieldType :: FieldType -> Text
outputTypeGuardForFieldType (LiteralType (LiteralString text)) = mconcat ["\"", text, "\""]
outputTypeGuardForFieldType (LiteralType (LiteralInteger x)) = tshow x
outputTypeGuardForFieldType (LiteralType (LiteralFloat f)) = tshow f
outputTypeGuardForFieldType (LiteralType (LiteralBoolean b)) = bool "false" "true" b
outputTypeGuardForFieldType (BasicType basicType) = outputTypeGuardForBasicType basicType
outputTypeGuardForFieldType (ComplexType complexType) = outputTypeGuardForComplexType complexType
outputTypeGuardForFieldType (DefinitionReferenceType definitionReference) =
  outputTypeGuardForDefinitionReference definitionReference
outputTypeGuardForFieldType (RecursiveReferenceType (DefinitionName name)) = "is" <> name
outputTypeGuardForFieldType (TypeVariableReferenceType (TypeVariable name)) = "is" <> name

outputTypeGuardForDefinitionReference :: DefinitionReference -> Text
outputTypeGuardForDefinitionReference (DefinitionReference (TypeDefinition (DefinitionName name) _typeData)) =
  "is" <> name
outputTypeGuardForDefinitionReference
  ( ImportedDefinitionReference
      (ModuleName moduleName)
      (DefinitionName name)
      _typeData
    ) =
    mconcat [moduleName, ".is", name]
outputTypeGuardForDefinitionReference _ = ""

outputTypeGuardForBasicType :: BasicTypeValue -> Text
outputTypeGuardForBasicType BasicString = "svt.isString"
outputTypeGuardForBasicType U8 = "svt.isNumber"
outputTypeGuardForBasicType U16 = "svt.isNumber"
outputTypeGuardForBasicType U32 = "svt.isNumber"
outputTypeGuardForBasicType U64 = "svt.isNumber"
outputTypeGuardForBasicType U128 = "svt.isNumber"
outputTypeGuardForBasicType I8 = "svt.isNumber"
outputTypeGuardForBasicType I16 = "svt.isNumber"
outputTypeGuardForBasicType I32 = "svt.isNumber"
outputTypeGuardForBasicType I64 = "svt.isNumber"
outputTypeGuardForBasicType I128 = "svt.isNumber"
outputTypeGuardForBasicType F32 = "svt.isNumber"
outputTypeGuardForBasicType F64 = "svt.isNumber"
outputTypeGuardForBasicType Boolean = "svt.isBoolean"

outputTypeGuardForComplexType :: ComplexTypeValue -> Text
outputTypeGuardForComplexType (ArrayType _size typeData) =
  mconcat ["svt.arrayOf(", outputTypeGuardForFieldType typeData, ")"]
outputTypeGuardForComplexType (SliceType typeData) =
  mconcat ["svt.arrayOf(", outputTypeGuardForFieldType typeData, ")"]
outputTypeGuardForComplexType (PointerType typeData) =
  outputTypeGuardForFieldType typeData
outputTypeGuardForComplexType (OptionalType typeData) =
  mconcat ["svt.optional(", outputTypeGuardForFieldType typeData, ")"]

outputUnion :: Text -> TagType -> UnionType -> Text
outputUnion name tagType unionType =
  let caseUnionOutput = outputCaseUnion name (constructorsFrom unionType)
      constructorsFrom (PlainUnion constructors) = constructors
      constructorsFrom (GenericUnion _typeVariables constructors) = constructors
      unionTagEnumerationOutput = outputUnionTagEnumeration name (constructorsFrom unionType)
      caseTypesOutput = outputCaseTypes name tagType (constructorsFrom unionType)
      caseConstructorOutput = outputCaseConstructors name tagType (constructorsFrom unionType)
      unionTypeGuardOutput = outputUnionTypeGuard name (constructorsFrom unionType)
      caseTypeGuardOutput = outputCaseTypeGuards name tagType (constructorsFrom unionType)
      unionValidatorOutput = outputUnionValidator tagType name (constructorsFrom unionType)
      caseValidatorOutput = outputCaseValidators tagType name (constructorsFrom unionType)
   in mconcat
        [ caseUnionOutput,
          "\n\n",
          unionTagEnumerationOutput,
          "\n\n",
          caseTypesOutput,
          "\n\n",
          caseConstructorOutput,
          "\n\n",
          unionTypeGuardOutput,
          "\n\n",
          caseTypeGuardOutput,
          "\n\n",
          unionValidatorOutput,
          "\n\n",
          caseValidatorOutput
        ]

newtype FunctionPrefix = FunctionPrefix Text

newtype ReturnType = ReturnType (Text -> Text)

newtype ReturnExpression = ReturnExpression (Text -> TagType -> [Constructor] -> Text)

outputUnionFunction ::
  FunctionPrefix ->
  ReturnType ->
  ReturnExpression ->
  TagType ->
  Text ->
  [Constructor] ->
  Text
outputUnionFunction
  (FunctionPrefix prefix)
  (ReturnType returnType)
  (ReturnExpression returnExpression)
  tagType
  unionName
  constructors =
    mconcat
      [ mconcat ["export function ", prefix, unionName, "(value: unknown): ", returnType unionName, " {\n"],
        mconcat ["    return ", returnExpression unionName tagType constructors, ";\n"],
        "}"
      ]

outputUnionTypeGuard :: Text -> [Constructor] -> Text
outputUnionTypeGuard =
  let returnExpression _unionName' _tagType constructors' =
        let constructorTypeGuards =
              constructors'
                & fmap
                  (\(Constructor (ConstructorName constructorName) _payload) -> "is" <> constructorName)
                & Text.intercalate ", "
         in mconcat ["[", constructorTypeGuards, "].some((typePredicate) => typePredicate(value))"]
   in outputUnionFunction
        (FunctionPrefix "is")
        (ReturnType (\n -> "value is " <> n))
        (ReturnExpression returnExpression)
        NoTypeTag

outputUnionValidator :: TagType -> Text -> [Constructor] -> Text
outputUnionValidator tagType =
  let returnExpression unionName (StandardTypeTag (FieldName tag)) constructors' =
        let constructorTagValidators =
              constructors'
                & fmap
                  ( \(Constructor (ConstructorName constructorName) _payload) ->
                      let tagName = unionEnumConstructorTag unionName constructorName
                       in mconcat ["[", tagName, "]: ", "validate", constructorName]
                  )
                & Text.intercalate ", "
         in mconcat
              [ "svt.validateWithTypeTag<",
                unionName,
                ">(value, {",
                constructorTagValidators,
                "}, \"",
                tag,
                "\")"
              ]
   in outputUnionFunction
        (FunctionPrefix "validate")
        (ReturnType (\n -> "svt.ValidationResult<" <> n <> ">"))
        (ReturnExpression returnExpression)
        tagType

outputCaseTypeGuards :: Text -> TagType -> [Constructor] -> Text
outputCaseTypeGuards unionName typeTag =
  fmap (outputCaseTypeGuard unionName typeTag) >>> Text.intercalate "\n\n"

outputCaseTypeGuard :: Text -> TagType -> Constructor -> Text
outputCaseTypeGuard
  unionName
  (StandardTypeTag (FieldName tag))
  (Constructor (ConstructorName name) maybePayload) =
    let tagValue = unionEnumConstructorTag unionName name
        interface =
          mconcat $
            ["{", tag, ": ", tagValue]
              <> maybe ["}"] (\p -> [", data: ", outputTypeGuardForFieldType p, "}"]) maybePayload
     in mconcat
          [ mconcat ["export function is", name, "(value: unknown): value is ", name, " {\n"],
            mconcat ["    return svt.isInterface<", name, ">(value, ", interface, ");\n"],
            "}"
          ]

outputCaseValidators :: TagType -> Text -> [Constructor] -> Text
outputCaseValidators typeTag unionName =
  fmap (outputCaseValidator typeTag unionName) >>> Text.intercalate "\n\n"

outputCaseValidator :: TagType -> Text -> Constructor -> Text
outputCaseValidator
  (StandardTypeTag (FieldName tag))
  unionName
  (Constructor (ConstructorName name) maybePayload) =
    let tagValue = unionEnumConstructorTag unionName name
        interface =
          mconcat $
            ["{", tag, ": ", tagValue]
              <> maybe ["}"] (\p -> [", data: ", outputValidatorForFieldType p, "}"]) maybePayload
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

outputCaseUnion :: Text -> [Constructor] -> Text
outputCaseUnion name constructors =
  let cases =
        constructors
          & fmap (\(Constructor (ConstructorName constructorName) _fieldType) -> constructorName)
          & Text.intercalate " | "
   in mconcat ["export type ", name, " = ", cases, ";"]

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

outputCaseTypes :: Text -> TagType -> [Constructor] -> Text
outputCaseTypes unionName tagType constructors =
  constructors
    & fmap (outputCaseType unionName tagType)
    & Text.intercalate "\n\n"

outputCaseType :: Text -> TagType -> Constructor -> Text
outputCaseType
  unionName
  (StandardTypeTag (FieldName tagField))
  (Constructor (ConstructorName name) maybePayload) =
    let payloadLine = maybe "" (\p -> "    data: " <> outputFieldType p <> ";\n") maybePayload
     in mconcat
          [ mconcat ["export type ", name, " = {\n"],
            mconcat ["    ", tagField, ": ", unionEnumConstructorTag unionName name, ";\n"],
            payloadLine,
            "};"
          ]

outputCaseConstructors :: Text -> TagType -> [Constructor] -> Text
outputCaseConstructors unionName tagType constructors =
  constructors
    & fmap (outputCaseConstructor unionName tagType)
    & Text.intercalate "\n\n"

outputCaseConstructor :: Text -> TagType -> Constructor -> Text
outputCaseConstructor
  unionName
  (StandardTypeTag (FieldName tagField))
  (Constructor (ConstructorName name) maybePayload) =
    let argumentFieldAndType = maybe "" (\p -> "data: " <> outputFieldType p) maybePayload
     in mconcat
          [ mconcat ["export function ", name, "(", argumentFieldAndType, "): ", name, " {\n"],
            mconcat
              ( [ "    return {",
                  tagField,
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

outputField :: StructField -> Text
outputField (StructField (FieldName name) fieldType) =
  mconcat ["    ", name, ": ", outputFieldType fieldType, ";\n"]

outputFieldType :: FieldType -> Text
outputFieldType (LiteralType (LiteralString text)) = mconcat ["\"", text, "\""]
outputFieldType (LiteralType (LiteralInteger x)) = tshow x
outputFieldType (LiteralType (LiteralFloat f)) = tshow f
outputFieldType (LiteralType (LiteralBoolean b)) = bool "false" "true" b
outputFieldType (BasicType basicType) = outputBasicType basicType
outputFieldType (ComplexType (OptionalType fieldType)) =
  mconcat [outputFieldType fieldType, " | null | undefined"]
outputFieldType (ComplexType (ArrayType _size fieldType@(ComplexType (OptionalType _)))) =
  mconcat ["(", outputFieldType fieldType, ")", "[]"]
outputFieldType (ComplexType (ArrayType _size fieldType)) =
  mconcat [outputFieldType fieldType, "[]"]
outputFieldType (ComplexType (SliceType fieldType)) =
  mconcat [outputFieldType fieldType, "[]"]
outputFieldType (ComplexType (PointerType fieldType)) = outputFieldType fieldType
outputFieldType (RecursiveReferenceType (DefinitionName name)) = name
outputFieldType
  (DefinitionReferenceType (DefinitionReference (TypeDefinition (DefinitionName name) _))) =
    name
outputFieldType
  ( DefinitionReferenceType
      ( ImportedDefinitionReference
          (ModuleName moduleName)
          (DefinitionName name)
          _typeData
        )
    ) =
    mconcat [moduleName, ".", name]
outputFieldType _ = ""

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

-- = LiteralType !LiteralTypeValue

-- | BasicType !BasicTypeValue
-- | ComplexType !ComplexTypeValue
-- | DefinitionReferenceType !DefinitionReference
-- | RecursiveReferenceType !DefinitionName
-- | TypeVariableReferenceType !TypeVariable
