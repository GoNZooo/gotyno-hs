module CodeGeneration.TypeScript (outputModule) where

import CodeGeneration.Utilities (typeVariablesFrom, upperCaseFirstCharacter)
import RIO
import qualified RIO.Text as Text
import Types

outputModule :: Module -> Text
outputModule Module {definitions, imports, declarationNames} =
  let definitionOutput = definitions & mapMaybe outputDefinition & Text.intercalate "\n\n"
      importsOutput = imports & fmap outputImport & mconcat
      outputImport (Import Module {name = ModuleName name}) =
        mconcat ["import * as ", name, " from \"./", name, "\";\n\n"]
      declarationImportOutput =
        declarationNames
          & fmap
            ( \(ModuleName name) ->
                mconcat ["import * as ", name, " from \"./", name, "\";"]
            )
          & Text.intercalate "\n"
   in mconcat
        [ modulePrelude,
          "\n\n",
          importsOutput,
          declarationImportOutput,
          if null declarationNames then "" else "\n\n",
          definitionOutput
        ]

modulePrelude :: Text
modulePrelude = "import * as svt from \"simple-validation-tools\";"

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
outputDefinition
  ( TypeDefinition
      (DefinitionName _name)
      (DeclaredType _moduleName _typeVariables)
    ) =
    Nothing

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
  (EmbeddedConstructor (ConstructorName name) Nothing) =
    let tagName = unionEnumConstructorTag unionName name
        interface = mconcat ["{", tag, ": ", tagName, "}"]
        constructorName = upperCaseFirstCharacter name
     in mconcat
          [ mconcat
              [ "export function is",
                constructorName,
                "(value: unknown): value is ",
                constructorName,
                " {\n"
              ],
            mconcat ["    return svt.isInterface<", constructorName, ">(value, ", interface, ");\n"],
            "}"
          ]
outputEmbeddedCaseTypeGuard
  (FieldName tag)
  unionName
  (EmbeddedConstructor (ConstructorName name) (Just reference)) =
    let fields = structFieldsFromReference reference
        tagName = unionEnumConstructorTag unionName name
        interface = mconcat ["{", tag, ": ", tagName, ", ", fieldTypeGuards, "}"]
        fieldTypeGuards = fields & fmap outputStructTypeGuardForField & Text.intercalate ", "
        constructorName = upperCaseFirstCharacter name
     in mconcat
          [ mconcat
              [ "export function is",
                constructorName,
                "(value: unknown): value is ",
                constructorName,
                " {\n"
              ],
            mconcat ["    return svt.isInterface<", constructorName, ">(value, ", interface, ");\n"],
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
  (EmbeddedConstructor (ConstructorName name) Nothing) =
    let tagName = unionEnumConstructorTag unionName name
        interface = mconcat ["{", tag, ": ", tagName, "}"]
        constructorName = upperCaseFirstCharacter name
     in mconcat
          [ mconcat
              [ "export function validate",
                constructorName,
                "(value: unknown): svt.ValidationResult<",
                constructorName,
                "> {\n"
              ],
            mconcat ["    return svt.validate<", constructorName, ">(value, ", interface, ");\n"],
            "}"
          ]
outputEmbeddedCaseValidator
  (FieldName tag)
  unionName
  (EmbeddedConstructor (ConstructorName name) (Just reference)) =
    let fields = structFieldsFromReference reference
        tagName = unionEnumConstructorTag unionName name
        interface = mconcat ["{", tag, ": ", tagName, ", ", fieldValidators, "}"]
        fieldValidators = fields & fmap outputValidatorForField & Text.intercalate ", "
        constructorName = upperCaseFirstCharacter name
     in mconcat
          [ mconcat
              [ "export function validate",
                constructorName,
                "(value: unknown): svt.ValidationResult<",
                constructorName,
                "> {\n"
              ],
            mconcat ["    return svt.validate<", constructorName, ">(value, ", interface, ");\n"],
            "}"
          ]

outputEmbeddedConstructorTypes :: Text -> FieldName -> [EmbeddedConstructor] -> Text
outputEmbeddedConstructorTypes unionName fieldName constructors =
  constructors & fmap (outputEmbeddedConstructorType unionName fieldName) & Text.intercalate "\n\n"

outputEmbeddedConstructorType :: Text -> FieldName -> EmbeddedConstructor -> Text
outputEmbeddedConstructorType
  unionName
  (FieldName tag)
  (EmbeddedConstructor (ConstructorName name) Nothing) =
    let tagFieldOutput = mconcat ["    ", tag, ": ", tagValue, ";"]
        tagValue = unionEnumConstructorTag unionName name
     in mconcat
          [ mconcat ["export type ", upperCaseFirstCharacter name, " = {\n"],
            mconcat [tagFieldOutput, "\n"],
            "};"
          ]
outputEmbeddedConstructorType
  unionName
  (FieldName tag)
  (EmbeddedConstructor (ConstructorName name) (Just fields)) =
    let fieldsOutput = fields & structFieldsFromReference & fmap outputField & Text.intercalate ""
        tagFieldOutput = mconcat ["    ", tag, ": ", tagValue, ";"]
        tagValue = unionEnumConstructorTag unionName name
     in mconcat
          [ mconcat ["export type ", upperCaseFirstCharacter name, " = {\n"],
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
  (EmbeddedConstructor (ConstructorName name) Nothing) =
    let constructorName = upperCaseFirstCharacter name
     in mconcat
          [ mconcat
              [ "export function ",
                constructorName,
                "(): ",
                constructorName,
                " {\n"
              ],
            mconcat
              [ "    return {",
                tag,
                ": ",
                unionEnumConstructorTag unionName name,
                "};\n"
              ],
            "}"
          ]
outputEmbeddedCaseConstructor
  unionName
  (FieldName tag)
  (EmbeddedConstructor (ConstructorName name) (Just definitionReference)) =
    let constructorName = upperCaseFirstCharacter name
     in mconcat
          [ mconcat
              [ "export function ",
                constructorName,
                "(data: ",
                outputDefinitionReference definitionReference,
                "): ",
                constructorName,
                " {\n"
              ],
            mconcat
              [ "    return {",
                tag,
                ": ",
                unionEnumConstructorTag unionName name,
                ", ...data};\n"
              ],
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
  Constructor name (DefinitionReferenceType <$> reference)

outputUntaggedUnion :: Text -> [FieldType] -> Text
outputUntaggedUnion unionName cases =
  let typeOutput = mconcat ["export type ", unionName, " = ", unionOutput, ";"]
      unionOutput = cases & fmap outputFieldType & Text.intercalate " | "
      typeGuardOutput = outputUntaggedUnionTypeGuard unionName cases
      validatorOutput = outputUntaggedUnionValidator unionName cases
   in Text.intercalate "\n\n" [typeOutput, typeGuardOutput, validatorOutput]

outputUntaggedUnionTypeGuard :: Text -> [FieldType] -> Text
outputUntaggedUnionTypeGuard name cases =
  let typeGuards = cases & fmap outputTypeGuardForFieldType & Text.intercalate ", "
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
  let export = mconcat ["export type ", name, " = {\n"]
      fieldsOutput = fields & fmap outputField & mconcat
      typeGuardOutput = outputStructTypeGuard name fields []
      validatorOutput = outputStructValidator name fields []
   in mconcat [export, fieldsOutput, "};\n\n", typeGuardOutput, "\n\n", validatorOutput]

outputGenericStruct :: Text -> [TypeVariable] -> [StructField] -> Text
outputGenericStruct name typeVariables fields =
  let fullName = name <> joinTypeVariables typeVariables
      typeOutput =
        mconcat
          [ mconcat ["export type ", fullName, " = {\n"],
            fieldsOutput,
            "};"
          ]
      fieldsOutput = fields & fmap outputField & mconcat
      typeGuardOutput = outputStructTypeGuard name fields typeVariables
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
outputValidatorForDefinitionReference
  ( DefinitionReference
      ( TypeDefinition
          (DefinitionName name)
          (DeclaredType (ModuleName moduleName) _appliedTypes)
        )
    ) =
    mconcat [moduleName, ".validate", upperCaseFirstCharacter name]
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
      ( TypeDefinition
          (DefinitionName name)
          ( DeclaredType
              (ModuleName moduleName)
              _appliedTypes
            )
        )
    ) =
    let appliedValidators = appliedTypes & fmap outputValidatorForFieldType & Text.intercalate ", "
     in mconcat [moduleName, ".validate", name, "(", appliedValidators, ")"]
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
outputValidatorForDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedValidators = appliedTypes & fmap outputValidatorForFieldType & Text.intercalate ", "
        maybeAppliedTypes = if null appliedTypes then "" else mconcat ["(", appliedValidators, ")"]
     in mconcat [moduleName, ".validate", upperCaseFirstCharacter name, maybeAppliedTypes]
outputValidatorForDefinitionReference
  (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
    mconcat [moduleName, ".validate", upperCaseFirstCharacter name]

outputValidatorForBasicType :: BasicTypeValue -> Text
outputValidatorForBasicType BasicString = "svt.validateString"
outputValidatorForBasicType U8 = "svt.validateNumber"
outputValidatorForBasicType U16 = "svt.validateNumber"
outputValidatorForBasicType U32 = "svt.validateNumber"
outputValidatorForBasicType U64 = "svt.validateBigInt"
outputValidatorForBasicType U128 = "svt.validateBigInt"
outputValidatorForBasicType I8 = "svt.validateNumber"
outputValidatorForBasicType I16 = "svt.validateNumber"
outputValidatorForBasicType I32 = "svt.validateNumber"
outputValidatorForBasicType I64 = "svt.validateBigInt"
outputValidatorForBasicType I128 = "svt.validateBigInt"
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

outputStructTypeGuard :: Text -> [StructField] -> [TypeVariable] -> Text
outputStructTypeGuard name fields typeVariables =
  let interface =
        "{" <> (fields & fmap outputStructTypeGuardForField & Text.intercalate ", ") <> "}"
   in if null typeVariables
        then
          mconcat
            [ mconcat
                [ mconcat ["export function is", name, "(value: unknown): value is ", name, " {\n"],
                  mconcat ["    return svt.isInterface<", name, ">(value, ", interface, ");\n"]
                ],
              "}"
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
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    mconcat [moduleName, ".is", name]
outputTypeGuardForDefinitionReference
  ( AppliedGenericReference
      appliedTypes
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedTypeGuards = appliedTypes & fmap outputTypeGuardForFieldType & Text.intercalate ", "
     in mconcat ["is", name, "(", appliedTypeGuards, ")"]
outputTypeGuardForDefinitionReference
  ( AppliedImportedGenericReference
      (ModuleName moduleName)
      (AppliedTypes appliedTypes)
      (TypeDefinition (DefinitionName name) _typeData)
    ) =
    let appliedTypeGuards = appliedTypes & fmap outputTypeGuardForFieldType & Text.intercalate ", "
     in mconcat [moduleName, ".is", name, "(", appliedTypeGuards, ")"]
outputTypeGuardForDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedTypeGuards = appliedTypes & fmap outputTypeGuardForFieldType & Text.intercalate ", "
        maybeAppliedTypes = if null appliedTypes then "" else mconcat ["(", appliedTypeGuards, ")"]
     in mconcat [moduleName, ".is", upperCaseFirstCharacter name, maybeAppliedTypes]
outputTypeGuardForDefinitionReference
  (DeclarationReference (ModuleName moduleName) (DefinitionName name)) =
    mconcat [moduleName, ".is", upperCaseFirstCharacter name]

outputTypeGuardForBasicType :: BasicTypeValue -> Text
outputTypeGuardForBasicType BasicString = "svt.isString"
outputTypeGuardForBasicType U8 = "svt.isNumber"
outputTypeGuardForBasicType U16 = "svt.isNumber"
outputTypeGuardForBasicType U32 = "svt.isNumber"
outputTypeGuardForBasicType U64 = "svt.isBigInt"
outputTypeGuardForBasicType U128 = "svt.isBigInt"
outputTypeGuardForBasicType I8 = "svt.isNumber"
outputTypeGuardForBasicType I16 = "svt.isNumber"
outputTypeGuardForBasicType I32 = "svt.isNumber"
outputTypeGuardForBasicType I64 = "svt.isBigInt"
outputTypeGuardForBasicType I128 = "svt.isBigInt"
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

outputUnion :: Text -> FieldName -> UnionType -> Text
outputUnion name typeTag unionType =
  let caseUnionOutput = outputCaseUnion name (constructorsFrom unionType) typeVariables
      constructorsFrom (PlainUnion constructors) = constructors
      constructorsFrom (GenericUnion _typeVariables constructors) = constructors
      unionTagEnumerationOutput = outputUnionTagEnumeration name (constructorsFrom unionType)
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
                      ( \(Constructor (ConstructorName constructorName) _payload) ->
                          "is" <> upperCaseFirstCharacter constructorName
                      )
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
                     in mconcat ["is", upperCaseFirstCharacter name, maybeParameters]
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
                 in mconcat ["[", tagName, "]: ", "validate", upperCaseFirstCharacter name]
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
              <> maybe ["}"] (\p -> [", data: ", outputTypeGuardForFieldType p, "}"]) maybePayload
     in if null typeVariables
          then
            mconcat
              [ mconcat ["export function is", upperCaseFirstCharacter name, "(value: unknown): value is ", name, " {\n"],
                mconcat ["    return svt.isInterface<", name, ">(value, ", interface, ");\n"],
                "}"
              ]
          else
            let fullName = upperCaseFirstCharacter name <> joinTypeVariables typeVariables
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
                    upperCaseFirstCharacter name,
                    "(value: unknown): svt.ValidationResult<",
                    name,
                    "> {\n"
                  ],
                mconcat ["    return svt.validate<", name, ">(value, ", interface, ");\n"],
                "}"
              ]
          else
            let fullName = upperCaseFirstCharacter name <> joinTypeVariables typeVariables
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
                mconcat [upperCaseFirstCharacter constructorName]
                  <> maybe
                    ""
                    (typeVariablesFrom >>> maybeJoinTypeVariables)
                    maybePayload
            )
          & Text.intercalate " | "
      maybeTypeVariables = if null typeVariables then "" else joinTypeVariables typeVariables
   in mconcat ["export type ", name, maybeTypeVariables, " = ", cases, ";"]

outputUnionTagEnumeration :: Text -> [Constructor] -> Text
outputUnionTagEnumeration name constructors =
  let constructorCasesOutput =
        constructors
          & fmap
            ( \(Constructor (ConstructorName constructorName) _payload) ->
                mconcat ["    ", upperCaseFirstCharacter constructorName, " = \"", constructorName, "\",\n"]
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
          [ mconcat ["export type ", upperCaseFirstCharacter name, maybeTypeVariables, " = {\n"],
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
unionEnumConstructorTag unionName constructorName =
  mconcat [unionName, "Tag.", upperCaseFirstCharacter constructorName]

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
outputFieldType (ComplexType (SliceType fieldType@(ComplexType (OptionalType _)))) =
  mconcat ["(", outputFieldType fieldType, ")", "[]"]
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
outputDefinitionReference
  ( GenericDeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
      (AppliedTypes appliedTypes)
    ) =
    let appliedTypesOutput = appliedTypes & fmap outputFieldType & Text.intercalate ", "
        maybeAppliedOutput =
          if null appliedTypes then "" else mconcat ["<", appliedTypesOutput, ">"]
     in mconcat [moduleName, ".", name, maybeAppliedOutput]
outputDefinitionReference
  ( DeclarationReference
      (ModuleName moduleName)
      (DefinitionName name)
    ) =
    mconcat [moduleName, ".", name]

outputBasicType :: BasicTypeValue -> Text
outputBasicType BasicString = "string"
outputBasicType U8 = "number"
outputBasicType U16 = "number"
outputBasicType U32 = "number"
outputBasicType U64 = "bigint"
outputBasicType U128 = "bigint"
outputBasicType I8 = "number"
outputBasicType I16 = "number"
outputBasicType I32 = "number"
outputBasicType I64 = "bigint"
outputBasicType I128 = "bigint"
outputBasicType F32 = "number"
outputBasicType F64 = "number"
outputBasicType Boolean = "boolean"

maybeJoinTypeVariables :: Maybe [TypeVariable] -> Text
maybeJoinTypeVariables = maybe "" joinTypeVariables

joinTypeVariables :: [TypeVariable] -> Text
joinTypeVariables typeVariables =
  typeVariables & fmap (\(TypeVariable t) -> t) & Text.intercalate ", " & (\o -> "<" <> o <> ">")
