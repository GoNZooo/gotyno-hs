module ParsingSpec where

import qualified CodeGeneration.TypeScript as TypeScript
import Parsing
import RIO
import qualified RIO.List as List
import qualified RIO.List.Partial as PartialList
import Test.Hspec
import Types

basicTypeScriptReferenceOutput :: IO Text
basicTypeScriptReferenceOutput = readFileUtf8 "./test/reference-output/basic.ts"

spec :: Text -> Spec
spec basicOutput = do
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
