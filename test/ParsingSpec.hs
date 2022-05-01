module ParsingSpec where

import qualified CodeGeneration.FSharp as FSharp
import qualified CodeGeneration.Python as Python
import qualified CodeGeneration.TypeScript as TypeScript
import Parsing
import RIO
import qualified RIO.List.Partial as PartialList
import Test.Hspec
import Types

data TypeScriptReferenceOutput = TypeScriptReferenceOutput
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
    basic :: !Text
  }

typeScriptReferenceOutput :: IO TypeScriptReferenceOutput
typeScriptReferenceOutput = do
  basic <- basicReferenceOutput "ts"
  import' <- importReferenceOutput "ts"
  hasGeneric <- hasGenericReferenceOutput "ts"
  generics <- genericsReferenceOutput "ts"
  gitHub <- gitHubReferenceOutput "ts"
  pure TypeScriptReferenceOutput {basic, import', hasGeneric, generics, gitHub}

fSharpReferenceOutput :: IO FSharpReferenceOutput
fSharpReferenceOutput = do
  basic <- basicReferenceOutput "fs"
  import' <- importReferenceOutput "fs"
  hasGeneric <- hasGenericReferenceOutput "fs"
  generics <- genericsReferenceOutput "fs"
  gitHub <- gitHubReferenceOutput "fs"
  pure FSharpReferenceOutput {basic, import', hasGeneric, generics, gitHub}

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
  pure PythonReferenceOutput {python, basic}

spec :: TypeScriptReferenceOutput -> FSharpReferenceOutput -> PythonReferenceOutput -> Spec
spec
  (TypeScriptReferenceOutput tsBasic tsImport tsHasGeneric tsGenerics tsGitHub)
  (FSharpReferenceOutput fsBasic fsImport fsHasGeneric fsGenerics fsGitHub)
  (PythonReferenceOutput pyPython pyBasic) = do
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
                { name = ModuleName "relaxedWhiteSpace",
                  imports = [],
                  declarationNames = [],
                  sourceFile = "test/examples/relaxedWhiteSpace.gotyno",
                  definitions = expectedDefinitions
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
                { name = ModuleName "crlf",
                  imports = [],
                  declarationNames = [],
                  sourceFile = "test/examples/crlf.gotyno",
                  definitions = expectedDefinitions
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
                { name = ModuleName "oneCharacterFieldNames",
                  imports = [],
                  declarationNames = [],
                  sourceFile = "test/examples/oneCharacterFieldNames.gotyno",
                  definitions = expectedDefinitions
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

    describe "Reference output" $ do
      it "Gives the correct parsed output for `basic.gotyno`" $ do
        Module {name, imports, definitions} <-
          (getRight >>> PartialList.head) <$> parseModules ["examples/basic.gotyno"]
        name `shouldBe` ModuleName "basic"
        imports `shouldBe` []
        length definitions `shouldBe` 13

      it "Mirrors reference output for `basic.gotyno`" $ do
        basicModule <- (getRight >>> PartialList.head) <$> parseModules ["examples/basic.gotyno"]
        let basicTypeScriptOutput = TypeScript.outputModule basicModule
            basicFSharpOutput = FSharp.outputModule basicModule
            basicPythonOutput = Python.outputModule basicModule
        basicTypeScriptOutput `shouldBe` tsBasic
        basicFSharpOutput `shouldBe` fsBasic
        basicPythonOutput `shouldBe` pyBasic

      it "Mirrors reference output for `importExample.gotyno`" $ do
        importModule <-
          (getRight >>> PartialList.last)
            <$> ( ["basic.gotyno", "importExample.gotyno"]
                    & fmap ("examples/" <>)
                    & parseModules
                )
        let importTypeScriptOutput = TypeScript.outputModule importModule
            importFSharpOutput = FSharp.outputModule importModule
        importTypeScriptOutput `shouldBe` tsImport
        importFSharpOutput `shouldBe` fsImport

      it "Mirrors reference output for `hasGeneric.gotyno`" $ do
        hasGenericModule <-
          (getRight >>> PartialList.head) <$> parseModules ["examples/hasGeneric.gotyno"]
        let hasGenericTypeScriptOutput = TypeScript.outputModule hasGenericModule
            hasGenericFSharpOutput = FSharp.outputModule hasGenericModule
        hasGenericTypeScriptOutput `shouldBe` tsHasGeneric
        hasGenericFSharpOutput `shouldBe` fsHasGeneric

      it "Mirrors reference output for `generics.gotyno`" $ do
        genericsModule <-
          (getRight >>> PartialList.last)
            <$> ( ["basic.gotyno", "hasGeneric.gotyno", "generics.gotyno"]
                    & fmap ("examples/" <>)
                    & parseModules
                )
        let genericsTypeScriptOutput = TypeScript.outputModule genericsModule
            genericsFSharpOutput = FSharp.outputModule genericsModule
        genericsTypeScriptOutput `shouldBe` tsGenerics
        genericsFSharpOutput `shouldBe` fsGenerics

      it "Mirrors reference output for `github.gotyno`" $ do
        gitHubModule <-
          (getRight >>> PartialList.head) <$> parseModules ["./examples/github.gotyno"]
        let gitHubTypeScriptOutput = TypeScript.outputModule gitHubModule
            gitHubFSharpOutput = FSharp.outputModule gitHubModule
        gitHubTypeScriptOutput `shouldBe` tsGitHub
        gitHubFSharpOutput `shouldBe` fsGitHub

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

getRight :: Either [String] r -> r
getRight (Right r) = r
getRight (Left e) = error $ mconcat e

shouldBeRight :: Either [String] r -> Expectation
shouldBeRight (Right _r) = pure ()
shouldBeRight (Left e) = error $ mconcat e
