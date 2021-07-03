module ParsingSpec where

import qualified CodeGeneration.FSharp as FSharp
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
    generics :: !Text
  }

data FSharpReferenceOutput = FSharpReferenceOutput
  { basic :: !Text,
    import' :: !Text,
    hasGeneric :: !Text,
    generics :: !Text
  }

typeScriptReferenceOutput :: IO TypeScriptReferenceOutput
typeScriptReferenceOutput = do
  basic <- basicReferenceOutput "ts"
  import' <- importReferenceOutput "ts"
  hasGeneric <- hasGenericReferenceOutput "ts"
  generics <- genericsReferenceOutput "ts"
  pure TypeScriptReferenceOutput {basic, import', hasGeneric, generics}

fSharpReferenceOutput :: IO FSharpReferenceOutput
fSharpReferenceOutput = do
  basic <- basicReferenceOutput "fs"
  import' <- importReferenceOutput "fs"
  hasGeneric <- hasGenericReferenceOutput "fs"
  generics <- genericsReferenceOutput "fs"
  pure FSharpReferenceOutput {basic, import', hasGeneric, generics}

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

spec :: TypeScriptReferenceOutput -> FSharpReferenceOutput -> Spec
spec
  (TypeScriptReferenceOutput tsBasic tsImport tsHasGeneric tsGenerics)
  (FSharpReferenceOutput fsBasic fsImport fsHasGeneric fsGenerics) = do
    describe "`parseModules`" $ do
      it "Parses and returns modules" $ do
        modules <- parseModules ["basic.gotyno"]
        length modules `shouldBe` 1

        modules' <- parseModules ["basic.gotyno", "importExample.gotyno"]
        length modules' `shouldBe` 2

        modules'' <-
          parseModules
            [ "basic.gotyno",
              "importExample.gotyno",
              "hasGeneric.gotyno",
              "generics.gotyno"
            ]
        length modules'' `shouldBe` 4

      it "Gives the correct parsed output for `basic.gotyno`" $ do
        Module {name, imports, definitions} <- PartialList.head <$> parseModules ["basic.gotyno"]
        name `shouldBe` ModuleName "basic"
        imports `shouldBe` []
        length definitions `shouldBe` 12

      it "Mirrors reference output for `basic.gotyno`" $ do
        basicModule <- PartialList.head <$> parseModules ["basic.gotyno"]
        let basicTypeScriptOutput = TypeScript.outputModule basicModule
            basicFSharpOutput = FSharp.outputModule basicModule
        basicTypeScriptOutput `shouldBe` tsBasic
        basicFSharpOutput `shouldBe` fsBasic

      it "Mirrors reference output for `importExample.gotyno`" $ do
        importModule <- PartialList.head <$> parseModules ["basic.gotyno", "importExample.gotyno"]
        let importTypeScriptOutput = TypeScript.outputModule importModule
            importFSharpOutput = FSharp.outputModule importModule
        importTypeScriptOutput `shouldBe` tsImport
        importFSharpOutput `shouldBe` fsImport

      it "Mirrors reference output for `hasGeneric.gotyno`" $ do
        hasGenericModule <- PartialList.head <$> parseModules ["hasGeneric.gotyno"]
        let hasGenericTypeScriptOutput = TypeScript.outputModule hasGenericModule
            hasGenericFSharpOutput = FSharp.outputModule hasGenericModule
        hasGenericTypeScriptOutput `shouldBe` tsHasGeneric
        hasGenericFSharpOutput `shouldBe` fsHasGeneric

      it "Mirrors reference output for `generics.gotyno`" $ do
        genericsModule <-
          PartialList.head <$> parseModules ["basic.gotyno", "hasGeneric.gotyno", "generics.gotyno"]
        let genericsTypeScriptOutput = TypeScript.outputModule genericsModule
            genericsFSharpOutput = FSharp.outputModule genericsModule
        genericsTypeScriptOutput `shouldBe` tsGenerics
        genericsFSharpOutput `shouldBe` fsGenerics
