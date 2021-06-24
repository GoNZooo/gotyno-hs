module ParsingSpec where

import qualified CodeGeneration.TypeScript as TypeScript
import Parsing
import RIO
import qualified RIO.List.Partial as PartialList
import Test.Hspec
import Types

basicTypeScriptReferenceOutput :: IO Text
basicTypeScriptReferenceOutput = readFileUtf8 "./test/reference-output/basic.ts"

importTypeScriptReferenceOutput :: IO Text
importTypeScriptReferenceOutput = readFileUtf8 "./test/reference-output/importExample.ts"

spec :: Text -> Text -> Spec
spec basicOutput importOutput = do
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
      length definitions `shouldBe` 4

    it "Mirrors reference output for `basic.gotyno`" $ do
      basicModuleText <- (PartialList.head >>> TypeScript.outputModule) <$> parseModules ["basic.gotyno"]
      basicModuleText `shouldBe` basicOutput

    it "Mirrors reference output for `importExample.gotyno`" $ do
      importModuleText <-
        (PartialList.head >>> TypeScript.outputModule)
          <$> parseModules ["basic.gotyno", "importExample.gotyno"]
      importModuleText `shouldBe` importOutput
