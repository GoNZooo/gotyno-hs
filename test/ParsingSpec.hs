module ParsingSpec where

import qualified CodeGeneration.DLang as DLang
import qualified CodeGeneration.FSharp as FSharp
import qualified CodeGeneration.Haskell as Haskell
import qualified CodeGeneration.Kotlin as Kotlin
import qualified CodeGeneration.Python as Python
import qualified CodeGeneration.TypeScript as TypeScript
import Parsing
import Qtility
import qualified RIO.List.Partial as PartialList
import Test.Hspec
import Types

-- { basicStruct :: !Text,
--   basicUnion :: !Text,
--   genericStruct :: !Text,
--   genericUnion :: !Text,
--   basicEnumeration :: !Text,
--   basicImport :: !Text,
--   basicOptional :: !Text,
data TypeScriptReferenceOutput = TypeScriptReferenceOutput
  { basicStruct :: !Text,
    basicUnion :: !Text,
    genericStruct :: !Text,
    genericUnion :: !Text,
    basic :: !Text,
    import' :: !Text,
    hasGeneric :: !Text,
    generics :: !Text,
    gitHub :: !Text
  }

data HaskellReferenceOutput = HaskellReferenceOutput
  { basic :: !Text,
    import' :: !Text,
    hasGeneric :: !Text,
    generics :: !Text,
    gitHub :: !Text
  }

data FSharpReferenceOutput = FSharpReferenceOutput
  { basic :: !Text,
    import' :: !Text,
    hasGeneric :: !Text,
    generics :: !Text,
    gitHub :: !Text
  }

data PythonReferenceOutput = PythonReferenceOutput
  { python :: !Text,
    basic :: !Text,
    generics :: !Text
  }

data KotlinReferenceOutput = KotlinReferenceOutput
  { basic :: !Text,
    import' :: !Text,
    hasGeneric :: !Text,
    generics :: !Text,
    gitHub :: !Text
  }

data DLangReferenceOutput = DLangReferenceOutput
  { basicStruct :: !Text,
    basicUnion :: !Text,
    genericStruct :: !Text,
    genericUnion :: !Text,
    basicEnumeration :: !Text,
    basicImport :: !Text,
    basicOptional :: !Text,
    basic :: !Text,
    import' :: !Text,
    hasGeneric :: !Text,
    generics :: !Text
  }

typeScriptReferenceOutput :: IO TypeScriptReferenceOutput
typeScriptReferenceOutput = do
  basicStruct <- basicStructReferenceOutput "ts"
  basicUnion <- basicUnionReferenceOutput "ts"
  genericStruct <- genericStructReferenceOutput "ts"
  genericUnion <- genericUnionReferenceOutput "ts"
  basic <- basicReferenceOutput "ts"
  import' <- importReferenceOutput "ts"
  hasGeneric <- hasGenericReferenceOutput "ts"
  generics <- genericsReferenceOutput "ts"
  gitHub <- gitHubReferenceOutput "ts"
  pure
    TypeScriptReferenceOutput
      { basicStruct,
        basicUnion,
        genericStruct,
        genericUnion,
        basic,
        import',
        hasGeneric,
        generics,
        gitHub
      }

haskellReferenceOutput :: IO HaskellReferenceOutput
haskellReferenceOutput = do
  basic <- basicReferenceOutput "hs"
  import' <- importReferenceOutput "hs"
  hasGeneric <- hasGenericReferenceOutput "hs"
  generics <- genericsReferenceOutput "hs"
  gitHub <- gitHubReferenceOutput "hs"
  pure HaskellReferenceOutput {basic, import', hasGeneric, generics, gitHub}

fSharpReferenceOutput :: IO FSharpReferenceOutput
fSharpReferenceOutput = do
  basic <- basicReferenceOutput "fs"
  import' <- importReferenceOutput "fs"
  hasGeneric <- hasGenericReferenceOutput "fs"
  generics <- genericsReferenceOutput "fs"
  gitHub <- gitHubReferenceOutput "fs"
  pure FSharpReferenceOutput {basic, import', hasGeneric, generics, gitHub}

kotlinReferenceOutput :: IO KotlinReferenceOutput
kotlinReferenceOutput = do
  basic <- basicReferenceOutput "kt"
  import' <- importReferenceOutput "kt"
  hasGeneric <- hasGenericReferenceOutput "kt"
  generics <- genericsReferenceOutput "kt"
  gitHub <- gitHubReferenceOutput "kt"
  pure KotlinReferenceOutput {basic, import', hasGeneric, generics, gitHub}

dLangReferenceOutput :: IO DLangReferenceOutput
dLangReferenceOutput = do
  basicStruct <- basicStructReferenceOutput "d"
  basicUnion <- basicUnionReferenceOutput "d"
  genericStruct <- genericStructReferenceOutput "d"
  genericUnion <- genericUnionReferenceOutput "d"
  basicEnumeration <- basicEnumerationReferenceOutput "d"
  basicImport <- basicImportReferenceOutput "d"
  basicOptional <- basicOptionalReferenceOutput "d"
  basic <- basicReferenceOutput "d"
  import' <- importReferenceOutput "d"
  hasGeneric <- hasGenericReferenceOutput "d"
  generics <- genericsReferenceOutput "d"
  pure
    DLangReferenceOutput
      { basicStruct,
        basicUnion,
        genericStruct,
        genericUnion,
        basicEnumeration,
        basicImport,
        basicOptional,
        basic,
        import',
        hasGeneric,
        generics
      }

basicStructReferenceOutput :: FilePath -> IO Text
basicStructReferenceOutput extension =
  readFileUtf8 $ "./test/reference-output/basicStruct." <> extension

basicUnionReferenceOutput :: FilePath -> IO Text
basicUnionReferenceOutput extension =
  readFileUtf8 $ "./test/reference-output/basicUnion." <> extension

genericStructReferenceOutput :: FilePath -> IO Text
genericStructReferenceOutput extension =
  readFileUtf8 $ "./test/reference-output/genericStruct." <> extension

genericUnionReferenceOutput :: FilePath -> IO Text
genericUnionReferenceOutput extension =
  readFileUtf8 $ "./test/reference-output/genericUnion." <> extension

basicEnumerationReferenceOutput :: FilePath -> IO Text
basicEnumerationReferenceOutput extension =
  readFileUtf8 $ "./test/reference-output/basicEnumeration." <> extension

basicImportReferenceOutput :: FilePath -> IO Text
basicImportReferenceOutput extension =
  readFileUtf8 $ "./test/reference-output/basicImport." <> extension

basicOptionalReferenceOutput :: FilePath -> IO Text
basicOptionalReferenceOutput extension =
  readFileUtf8 $ "./test/reference-output/basicOptional." <> extension

basicReferenceOutput :: FilePath -> IO Text
basicReferenceOutput extension = readFileUtf8 $ "./test/reference-output/basic." <> extension

importReferenceOutput :: FilePath -> IO Text
importReferenceOutput extension =
  readFileUtf8 $ "./test/reference-output/importExample." <> extension

hasGenericReferenceOutput :: FilePath -> IO Text
hasGenericReferenceOutput extension =
  readFileUtf8 $ "./test/reference-output/hasGeneric." <> extension

genericsReferenceOutput :: FilePath -> IO Text
genericsReferenceOutput extension =
  readFileUtf8 $ "./test/reference-output/generics." <> extension

gitHubReferenceOutput :: FilePath -> IO Text
gitHubReferenceOutput extension =
  readFileUtf8 $ "./test/reference-output/github." <> extension

pythonReferenceOutput :: IO PythonReferenceOutput
pythonReferenceOutput = do
  python <- readFileUtf8 "./test/reference-output/python.py"
  basic <- readFileUtf8 "./test/reference-output/basic.py"
  generics <- readFileUtf8 "./test/reference-output/generics.py"
  pure PythonReferenceOutput {python, basic, generics}

spec ::
  TypeScriptReferenceOutput ->
  HaskellReferenceOutput ->
  FSharpReferenceOutput ->
  PythonReferenceOutput ->
  KotlinReferenceOutput ->
  DLangReferenceOutput ->
  Spec
spec
  ( TypeScriptReferenceOutput
      tsBasicStruct
      tsBasicUnion
      tsGenericStruct
      tsGenericUnion
      tsBasic
      tsImport
      tsHasGeneric
      tsGenerics
      tsGitHub
    )
  (HaskellReferenceOutput hsBasic hsImport hsHasGeneric hsGenerics hsGitHub)
  (FSharpReferenceOutput fsBasic fsImport fsHasGeneric fsGenerics fsGitHub)
  (PythonReferenceOutput pyPython pyBasic pyGenerics)
  (KotlinReferenceOutput ktBasic ktImport ktHasGeneric ktGenerics ktGitHub)
  ( DLangReferenceOutput
      dBasicStruct
      dBasicUnion
      dGenericStruct
      dGenericUnion
      dBasicEnumeration
      dBasicImport
      dBasicOptional
      dBasic
      dImport
      dHasGeneric
      dGenerics
    ) = do
    describe "`parseModules`" $ do
      it "Parses and returns modules" $ do
        modules <- getRight <$> parseModules ["examples/basic.gotyno"]
        length modules `shouldBe` 1

        modules' <- getRight <$> parseModules ["examples/basic.gotyno", "examples/importExample.gotyno"]
        length modules' `shouldBe` 2

        modules'' <-
          getRight
            <$> parseModules
              [ "examples/basic.gotyno",
                "examples/importExample.gotyno",
                "examples/hasGeneric.gotyno",
                "examples/generics.gotyno"
              ]
        length modules'' `shouldBe` 4

      it "Allows two separate modules to declare the same type (by name)" $ do
        modules <-
          getRight
            <$> parseModules
              ["test/examples/declaration1.gotyno", "test/examples/declaration2.gotyno"]
        length modules `shouldBe` 2

    describe "Handles applied type parameters properly" $ do
      it "Errors out when trying to apply a non-generic type" $ do
        result <- parseModules ["test/examples/applyingNonGeneric.gotyno"]
        isLeft result `shouldBe` True
        case result of
          Left e ->
            PartialList.head e `shouldContain` "Type 'NotGeneric' expects 0 type parameters"
          Right _ ->
            error "We should not hit `Right` when expecting error"

      it "Errors out when not applying enough type parameters" $ do
        result <- parseModules ["test/examples/notApplyingEnoughGenericTypes.gotyno"]
        isLeft result `shouldBe` True
        case result of
          Left e ->
            PartialList.head e
              `shouldContain` "Type 'GenericUnion' expects 2 type parameters, 1 applied"
          Right _ ->
            error "We should not hit `Right` when expecting error"
      it "Handles one missing argument out of 2" $ do
        result <- parseModules ["test/examples/declaredGenerics1.gotyno"]
        isLeft result `shouldBe` True
        case result of
          Left e ->
            PartialList.head e
              `shouldContain` "Type 'GenericUnion' expects 2 type parameters, 1 applied"
          Right _ ->
            error "We should not hit `Right` when expecting error"
      it "Handles two missing arguments out of 2" $ do
        result <- parseModules ["test/examples/declaredGenerics2.gotyno"]
        isLeft result `shouldBe` True
        case result of
          Left e ->
            PartialList.head e
              `shouldContain` "Type 'GenericUnion' expects 2 type parameters, 0 applied"
          Right _ ->
            error "We should not hit `Right` when expecting error"
      it "Handles one missing arguments out of 1" $ do
        result <- parseModules ["test/examples/declaredGenerics3.gotyno"]
        isLeft result `shouldBe` True
        case result of
          Left e ->
            PartialList.head e
              `shouldContain` "Type 'GenericUnion' expects 1 type parameters, 0 applied"
          Right _ ->
            error "We should not hit `Right` when expecting error"
      it "Errors out when trying to apply a non-generic declared type" $ do
        result <- parseModules ["test/examples/declaredGenerics5.gotyno"]
        isLeft result `shouldBe` True
        case result of
          Left e ->
            PartialList.head e
              `shouldContain` "Type 'GenericUnion' expects 0 type parameters, 1 applied"
          Right _ ->
            error "We should not hit `Right` when expecting error"
      it "Errors out when trying to apply too few parameters to imported generic" $ do
        result <-
          parseModules
            [ "test/examples/result.gotyno",
              "test/examples/notEnoughAppliedImportedGenerics.gotyno"
            ]
        isLeft result `shouldBe` True
        case result of
          Left [e1] -> do
            e1 `shouldContain` "Type 'result.Result' expects 2 type parameters, 0 applied"
          Left errors ->
            error $ "More/less errors than expected: " <> show errors
          Right _ ->
            error "We should not hit `Right` when expecting error"
      it "Errors out when trying to apply too many type parameters to imported generic" $ do
        result <-
          parseModules
            [ "test/examples/result.gotyno",
              "test/examples/tooManyAppliedImportedTypeParameters.gotyno"
            ]
        isLeft result `shouldBe` True
        case result of
          Left [e1] -> do
            e1 `shouldContain` "Type 'result.PlainType' expects 0 type parameters"
          Left errors ->
            error $ "More/less errors than expected: " <> show errors
          Right _ ->
            error "We should not hit `Right` when expecting error"
      it "Gives correct result when all are applied" $ do
        result <- parseModules ["test/examples/declaredGenerics4.gotyno"]
        isRight result `shouldBe` True

    describe "Parser is less rigid about syntax than reference implementation" $ do
      it "Does not error out when extra whitespace is used in many places" $ do
        result <- parseModules ["test/examples/relaxedWhiteSpace.gotyno"]
        let expectedModule =
              Module
                { _moduleName = ModuleName "relaxedWhiteSpace",
                  _moduleImports = [],
                  _moduleDeclarationNames = [],
                  _moduleSourceFile = "test/examples/relaxedWhiteSpace.gotyno",
                  _moduleDefinitions = expectedDefinitions
                }
            expectedDefinitions =
              [ TypeDefinition
                  ( DefinitionName
                      "UsingExtraSpaces"
                  )
                  ( Struct
                      ( PlainStruct [StructField (FieldName "field") (BasicType BasicString)]
                      )
                  ),
                TypeDefinition
                  (DefinitionName "DefinitionWithoutManyNewlines")
                  (Struct (PlainStruct [StructField (FieldName "field2") (BasicType U32)])),
                TypeDefinition
                  (DefinitionName "SomeUnionName")
                  ( Union
                      (FieldName "type")
                      ( PlainUnion
                          [ Constructor (ConstructorName "One") (Just (BasicType U32)),
                            Constructor (ConstructorName "Two") Nothing
                          ]
                      )
                  ),
                TypeDefinition
                  (DefinitionName "Name")
                  ( EmbeddedUnion
                      (FieldName "kind")
                      [ EmbeddedConstructor (ConstructorName "NoPayload") Nothing,
                        EmbeddedConstructor
                          (ConstructorName "WithPayload")
                          ( Just
                              ( DefinitionReference
                                  ( TypeDefinition
                                      (DefinitionName "UsingExtraSpaces")
                                      ( Struct
                                          ( PlainStruct
                                              [ StructField
                                                  (FieldName "field")
                                                  (BasicType BasicString)
                                              ]
                                          )
                                      )
                                  )
                              )
                          )
                      ]
                  ),
                TypeDefinition
                  (DefinitionName "EnumName")
                  ( Enumeration
                      BasicString
                      [ EnumerationValue
                          (EnumerationIdentifier "value1")
                          (LiteralString "value1"),
                        EnumerationValue (EnumerationIdentifier "value2") (LiteralString "value1")
                      ]
                  )
              ]
        result `shouldBe` Right [expectedModule]
      it "Accepts CRLF as line endings without changing behavior" $ do
        result <- parseModules ["test/examples/crlf.gotyno"]
        let expectedModule =
              Module
                { _moduleName = ModuleName "crlf",
                  _moduleImports = [],
                  _moduleDeclarationNames = [],
                  _moduleSourceFile = "test/examples/crlf.gotyno",
                  _moduleDefinitions = expectedDefinitions
                }
            expectedDefinitions =
              [ TypeDefinition
                  ( DefinitionName
                      "UsingExtraSpaces"
                  )
                  ( Struct
                      ( PlainStruct [StructField (FieldName "field") (BasicType BasicString)]
                      )
                  ),
                TypeDefinition
                  (DefinitionName "DefinitionWithoutManyNewlines")
                  (Struct (PlainStruct [StructField (FieldName "field2") (BasicType U32)])),
                TypeDefinition
                  (DefinitionName "SomeUnionName")
                  ( Union
                      (FieldName "type")
                      ( PlainUnion
                          [ Constructor (ConstructorName "One") (Just (BasicType U32)),
                            Constructor (ConstructorName "Two") Nothing
                          ]
                      )
                  ),
                TypeDefinition
                  (DefinitionName "Name")
                  ( EmbeddedUnion
                      (FieldName "kind")
                      [ EmbeddedConstructor (ConstructorName "NoPayload") Nothing,
                        EmbeddedConstructor
                          (ConstructorName "WithPayload")
                          ( Just
                              ( DefinitionReference
                                  ( TypeDefinition
                                      (DefinitionName "UsingExtraSpaces")
                                      ( Struct
                                          ( PlainStruct
                                              [ StructField
                                                  (FieldName "field")
                                                  (BasicType BasicString)
                                              ]
                                          )
                                      )
                                  )
                              )
                          )
                      ]
                  ),
                TypeDefinition
                  (DefinitionName "EnumName")
                  ( Enumeration
                      BasicString
                      [ EnumerationValue
                          (EnumerationIdentifier "value1")
                          (LiteralString "value1"),
                        EnumerationValue (EnumerationIdentifier "value2") (LiteralString "value1")
                      ]
                  )
              ]
        result `shouldBe` Right [expectedModule]

      it "Allows spaces in enum string values" $ do
        result <- parseModules ["test/examples/enumWithSpaces.gotyno"]
        shouldBeRight result

      it "Supports one character long fieldnames" $ do
        let expectedModule =
              Module
                { _moduleName = ModuleName "oneCharacterFieldNames",
                  _moduleImports = [],
                  _moduleDeclarationNames = [],
                  _moduleSourceFile = "test/examples/oneCharacterFieldNames.gotyno",
                  _moduleDefinitions = expectedDefinitions
                }
            expectedDefinitions =
              [ TypeDefinition
                  (DefinitionName "Coordinates")
                  ( Struct
                      ( PlainStruct
                          [ StructField (FieldName "x") (BasicType F32),
                            StructField (FieldName "y") (BasicType F32)
                          ]
                      )
                  )
              ]

        parseModules ["test/examples/oneCharacterFieldNames.gotyno"]
          `shouldReturn` Right [expectedModule]

    describe "imports" $ do
      it "Shouldn't be able to use imports from previously imported file without explicit import" $ do
        result <-
          parseModules
            [ "test/examples/importOne.gotyno",
              "test/examples/importTwo.gotyno",
              "test/examples/importThree.gotyno"
            ]
        isLeft result `shouldBe` True

    describe "Type parameter regression tests" $ do
      it "Should be able to parse definition name when it starts with a type parameter char" $ do
        result <- parseModules ["test/examples/typeVariableParsingAfterDefinitionReference.gotyno"]
        shouldBeRight result

    describe "Reference output" $ do
      it "Gives the correct parsed output for `basic.gotyno`" $ do
        Module {_moduleName = name, _moduleImports = imports, _moduleDefinitions = definitions} <-
          (getRight >>> PartialList.head) <$> parseModules ["examples/basic.gotyno"]
        name `shouldBe` ModuleName "basic"
        imports `shouldBe` []
        length definitions `shouldBe` 13

      it "Mirrors reference output for `basicStruct.gotyno`" $ do
        basicStructModule <-
          (getRight >>> PartialList.head) <$> parseModules ["examples/basicStruct.gotyno"]
        TypeScript.outputModule basicStructModule `shouldBe` tsBasicStruct
        DLang.outputModule basicStructModule `shouldBe` dBasicStruct

      it "Mirrors reference output for `basicUnion.gotyno`" $ do
        basicUnionModule <-
          (getRight >>> PartialList.head) <$> parseModules ["examples/basicUnion.gotyno"]
        TypeScript.outputModule basicUnionModule `shouldBe` tsBasicUnion
        DLang.outputModule basicUnionModule `shouldBe` dBasicUnion

      it "Mirrors reference output for `genericStruct.gotyno`" $ do
        genericStructModule <-
          (getRight >>> PartialList.head) <$> parseModules ["examples/genericStruct.gotyno"]
        TypeScript.outputModule genericStructModule `shouldBe` tsGenericStruct
        DLang.outputModule genericStructModule `shouldBe` dGenericStruct

      it "Mirrors reference output for `genericUnion.gotyno`" $ do
        genericUnionModule <-
          (getRight >>> PartialList.head) <$> parseModules ["examples/genericUnion.gotyno"]
        TypeScript.outputModule genericUnionModule `shouldBe` tsGenericUnion
        DLang.outputModule genericUnionModule `shouldBe` dGenericUnion

      it "Mirrors reference output for `basicEnumeration.gotyno`" $ do
        enumerationModule <-
          (getRight >>> PartialList.head) <$> parseModules ["examples/basicEnumeration.gotyno"]
        DLang.outputModule enumerationModule `shouldBe` dBasicEnumeration

      it "Mirrors reference output for `basicImport.gotyno`" $ do
        basicImportModule <-
          (getRight >>> PartialList.last)
            <$> parseModules ["examples/basicStruct.gotyno", "examples/basicImport.gotyno"]
        DLang.outputModule basicImportModule `shouldBe` dBasicImport

      it "Mirrors reference output for `basicOptional.gotyno`" $ do
        basicOptionalModule <-
          (getRight >>> PartialList.head) <$> parseModules ["examples/basicOptional.gotyno"]
        DLang.outputModule basicOptionalModule `shouldBe` dBasicOptional

      it "Mirrors reference output for `basic.gotyno`" $ do
        basicModule <- (getRight >>> PartialList.head) <$> parseModules ["examples/basic.gotyno"]
        TypeScript.outputModule basicModule `shouldBe` tsBasic
        Haskell.outputModule basicModule `shouldBe` hsBasic
        FSharp.outputModule basicModule `shouldBe` fsBasic
        Python.outputModule basicModule `shouldBe` pyBasic
        Kotlin.outputModule basicModule `shouldBe` ktBasic
        DLang.outputModule basicModule `shouldBe` dBasic

      it "Mirrors reference output for `importExample.gotyno`" $ do
        importModule <-
          (getRight >>> PartialList.last)
            <$> ( ["basic.gotyno", "importExample.gotyno"]
                    & fmap ("examples/" <>)
                    & parseModules
                )
        TypeScript.outputModule importModule `shouldBe` tsImport
        Haskell.outputModule importModule `shouldBe` hsImport
        FSharp.outputModule importModule `shouldBe` fsImport
        Kotlin.outputModule importModule `shouldBe` ktImport
        DLang.outputModule importModule `shouldBe` dImport

      it "Mirrors reference output for `hasGeneric.gotyno`" $ do
        hasGenericModule <-
          (getRight >>> PartialList.head) <$> parseModules ["examples/hasGeneric.gotyno"]
        TypeScript.outputModule hasGenericModule `shouldBe` tsHasGeneric
        Haskell.outputModule hasGenericModule `shouldBe` hsHasGeneric
        FSharp.outputModule hasGenericModule `shouldBe` fsHasGeneric
        Kotlin.outputModule hasGenericModule `shouldBe` ktHasGeneric
        DLang.outputModule hasGenericModule `shouldBe` dHasGeneric

      it "Mirrors reference output for `generics.gotyno`" $ do
        genericsModule <-
          (getRight >>> PartialList.last)
            <$> ( ["basic.gotyno", "hasGeneric.gotyno", "generics.gotyno"]
                    & fmap ("examples/" <>)
                    & parseModules
                )
        TypeScript.outputModule genericsModule `shouldBe` tsGenerics
        Haskell.outputModule genericsModule `shouldBe` hsGenerics
        FSharp.outputModule genericsModule `shouldBe` fsGenerics
        Python.outputModule genericsModule `shouldBe` pyGenerics
        Kotlin.outputModule genericsModule `shouldBe` ktGenerics
        DLang.outputModule genericsModule `shouldBe` dGenerics

      it "Mirrors reference output for `github.gotyno`" $ do
        gitHubModule <-
          (getRight >>> PartialList.head) <$> parseModules ["./examples/github.gotyno"]
        TypeScript.outputModule gitHubModule `shouldBe` tsGitHub
        Haskell.outputModule gitHubModule `shouldBe` hsGitHub
        FSharp.outputModule gitHubModule `shouldBe` fsGitHub
        Kotlin.outputModule gitHubModule `shouldBe` ktGitHub

      it "Basic `python.gotyno` module is output correctly" $ do
        pythonModule <-
          (getRight >>> PartialList.last)
            <$> parseModules ["examples/basic.gotyno", "examples/python.gotyno"]
        let pythonPythonOutput = Python.outputModule pythonModule
        pythonPythonOutput `shouldBe` pyPython
    describe "Misc. expected behavior" $ do
      it "Allows you to have a field that is an array of arrays of `?String`" $ do
        arrayOfArraysOfNullableStringTsOutput <- readFileUtf8 "./test/reference-output/arrayOfArraysOfNullableStrings.ts"
        module' <-
          (getRight >>> PartialList.last)
            <$> parseModules ["test/examples/arrayOfArraysOfNullableString.gotyno"]
        let importTypeScriptOutput = TypeScript.outputModule module'
        importTypeScriptOutput `shouldBe` arrayOfArraysOfNullableStringTsOutput

    describe "Specific bugs in Python output" $ do
      it "Does not emit `to_json` in Python when a constructor has no type variable in payload" $ do
        exceptionNotificationPythonReferenceOutput <-
          readFileUtf8 "./test/reference-output/exceptionNotification.py"
        exceptionNotificationModule <-
          (getRight >>> PartialList.head)
            <$> parseModules ["./test/examples/exceptionNotification.gotyno"]
        let exceptionNotificationPythonOutput = Python.outputModule exceptionNotificationModule
        exceptionNotificationPythonOutput `shouldBe` exceptionNotificationPythonReferenceOutput

      it "Emits module name in `to_json` interface for untagged unions with declared types" $ do
        untaggedUnionWithDeclarationPythonReferenceOutput <-
          readFileUtf8 "./test/reference-output/untaggedUnionWithDeclaration.py"
        untaggedUnionWithDeclarationModule <-
          (getRight >>> PartialList.head)
            <$> parseModules ["./test/examples/untaggedUnionWithDeclaration.gotyno"]
        let untaggedUnionWithDeclarationPythonOutput =
              Python.outputModule untaggedUnionWithDeclarationModule
        untaggedUnionWithDeclarationPythonOutput
          `shouldBe` untaggedUnionWithDeclarationPythonReferenceOutput

      it "Translates 'None' constructor to 'None_' automatically" $ do
        optionTypePythonReferenceOutput <- readFileUtf8 "./test/reference-output/optionType.py"
        optionTypeModule <-
          (getRight >>> PartialList.head)
            <$> parseModules ["./test/examples/optionType.gotyno"]
        let optionTypePythonOutput = Python.outputModule optionTypeModule
        optionTypePythonOutput `shouldBe` optionTypePythonReferenceOutput

      it "Translates `from` field name into `from_`" $ do
        fromFieldPythonReferenceOutput <- readFileUtf8 "./test/reference-output/fromField.py"
        fromFieldModule <-
          (getRight >>> PartialList.head)
            <$> parseModules ["./test/examples/fromField.gotyno"]
        let fromFieldPythonOutput = Python.outputModule fromFieldModule
        fromFieldPythonOutput `shouldBe` fromFieldPythonReferenceOutput

      it "Refers to interface of untagged union instead of name of type when validating" $ do
        untaggedUnionValidatorPythonReferenceOutput <-
          readFileUtf8 "./test/reference-output/untaggedUnionValidator.py"
        untaggedUnionValidatorModule <-
          (getRight >>> PartialList.head)
            <$> parseModules ["./test/examples/untaggedUnionValidator.gotyno"]
        let untaggedUnionValidatorPythonOutput = Python.outputModule untaggedUnionValidatorModule
        untaggedUnionValidatorPythonOutput `shouldBe` untaggedUnionValidatorPythonReferenceOutput

getRight :: Either [String] r -> r
getRight (Right r) = r
getRight (Left e) = error $ mconcat e

shouldBeRight :: Either [String] r -> Expectation
shouldBeRight (Right _r) = pure ()
shouldBeRight (Left e) = error $ mconcat e
