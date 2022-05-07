module HaskellOutputSpec where

import qualified CodeGeneration.Haskell as Haskell
import Parsing
import RIO
import qualified RIO.List.Partial as PartialList
import qualified RIO.Text as Text
import Test.Hspec

spec :: Spec
spec = do
  describe "`Haskell.outputModule`" $ do
    it "should output a plain struct" $ do
      let expectedOutput =
            Text.intercalate
              "\n"
              [ "{-# LANGUAGE StrictData #-}",
                "{-# LANGUAGE TemplateHaskell #-}",
                "",
                "module GotynoOutput.HaskellExampleStruct where",
                "",
                "import Data.Aeson (FromJSON (..), ToJSON (..))",
                "import qualified Data.Aeson as JSON",
                "import GHC.Generics (Generic)",
                "import qualified Gotyno.Helpers as Helpers",
                "import Qtility",
                "",
                "data StructOne = StructOne",
                "  { _structOneUnsigned_integer_field :: Int,",
                "    _structOneStringField :: Text,",
                "    _structOneBig_integer_field :: Helpers.BigInteger",
                "  }",
                "  deriving (Eq, Show, Generic)",
                "",
                "deriveLensAndJSON ''StructOne"
              ]
      Right [parsedModule] <- parseModules ["test/examples/haskellExampleStruct.gotyno"]
      Haskell.outputModule parsedModule `shouldBe` expectedOutput

    it "should output a generic struct" $ do
      let expectedOutput =
            Text.intercalate
              "\n"
              [ "{-# LANGUAGE StrictData #-}",
                "{-# LANGUAGE TemplateHaskell #-}",
                "",
                "module GotynoOutput.HaskellExampleGenericStruct where",
                "",
                "import Data.Aeson (FromJSON (..), ToJSON (..))",
                "import qualified Data.Aeson as JSON",
                "import GHC.Generics (Generic)",
                "import qualified Gotyno.Helpers as Helpers",
                "import Qtility",
                "",
                "data Holder t u = Holder",
                "  { _holderField1 :: t,",
                "    _holderField2 :: u",
                "  }",
                "  deriving (Eq, Show, Generic)",
                "",
                "deriveLensAndJSON ''Holder"
              ]
      Right [parsedModule] <- parseModules ["test/examples/haskellExampleGenericStruct.gotyno"]
      Haskell.outputModule parsedModule `shouldBe` expectedOutput

    it "should output a plain union" $ do
      let expectedOutput =
            Text.intercalate
              "\n"
              [ "{-# LANGUAGE StrictData #-}",
                "{-# LANGUAGE TemplateHaskell #-}",
                "",
                "module GotynoOutput.HaskellExamplePlainUnion where",
                "",
                "import Data.Aeson (FromJSON (..), ToJSON (..))",
                "import qualified Data.Aeson as JSON",
                "import GHC.Generics (Generic)",
                "import qualified Gotyno.Helpers as Helpers",
                "import Qtility",
                "",
                "data ExamplePlainUnion",
                "  = ConstructorOne Text",
                "  | ConstructorTwo Helpers.BigInteger",
                "  | LowerCaseConstructor",
                "  deriving (Eq, Show, Generic)",
                "",
                "instance ToJSON ExamplePlainUnion where",
                "  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions \"type\"",
                "",
                "instance FromJSON ExamplePlainUnion where",
                "  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions \"type\"",
                "",
                "makeLenses ''ExamplePlainUnion"
              ]
      Right [parsedModule] <- parseModules ["test/examples/haskellExamplePlainUnion.gotyno"]
      Haskell.outputModule parsedModule `shouldBe` expectedOutput

    it "should output a generic union" $ do
      let expectedOutput =
            Text.intercalate
              "\n"
              [ "{-# LANGUAGE StrictData #-}",
                "{-# LANGUAGE TemplateHaskell #-}",
                "",
                "module GotynoOutput.HaskellExampleGenericUnion where",
                "",
                "import Data.Aeson (FromJSON (..), ToJSON (..))",
                "import qualified Data.Aeson as JSON",
                "import GHC.Generics (Generic)",
                "import qualified Gotyno.Helpers as Helpers",
                "import Qtility",
                "",
                "data ExampleGenericUnion t u",
                "  = ConstructorOne t",
                "  | ConstructorTwo u",
                "  deriving (Eq, Show, Generic)",
                "",
                "instance (ToJSON t, ToJSON u) => ToJSON (ExampleGenericUnion t u) where",
                "  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions \"type\"",
                "",
                "instance (FromJSON t, FromJSON u) => FromJSON (ExampleGenericUnion t u) where",
                "  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions \"type\"",
                "",
                "makeLenses ''ExampleGenericUnion"
              ]
      Right [parsedModule] <- parseModules ["test/examples/haskellExampleGenericUnion.gotyno"]
      Haskell.outputModule parsedModule `shouldBe` expectedOutput

    it "should output an untagged union" $ do
      let expectedOutput =
            Text.intercalate
              "\n"
              [ "{-# LANGUAGE StrictData #-}",
                "{-# LANGUAGE TemplateHaskell #-}",
                "",
                "module GotynoOutput.HaskellExampleUntaggedUnion where",
                "",
                "import Data.Aeson (FromJSON (..), ToJSON (..))",
                "import qualified Data.Aeson as JSON",
                "import GHC.Generics (Generic)",
                "import qualified Gotyno.Helpers as Helpers",
                "import Qtility",
                "",
                "data ExampleUntaggedUnion",
                "  = ExampleUntaggedUnionString Text",
                "  | ExampleUntaggedUnionF64 Double",
                "  | ExampleUntaggedUnionBoolean Bool",
                "  deriving (Eq, Show, Generic)",
                "",
                "deriveLensAndJSON' 'Helpers.untaggedUnionOptions ''ExampleUntaggedUnion"
              ]
      Right [parsedModule] <- parseModules ["test/examples/haskellExampleUntaggedUnion.gotyno"]
      Haskell.outputModule parsedModule `shouldBe` expectedOutput

    it "should output an embedded union" $ do
      let expectedOutput =
            Text.intercalate
              "\n"
              [ "{-# LANGUAGE StrictData #-}",
                "{-# LANGUAGE TemplateHaskell #-}",
                "",
                "module GotynoOutput.HaskellExampleEmbeddedUnion where",
                "",
                "import Data.Aeson (FromJSON (..), ToJSON (..))",
                "import qualified Data.Aeson as JSON",
                "import GHC.Generics (Generic)",
                "import qualified Gotyno.Helpers as Helpers",
                "import Qtility",
                "",
                "data Payload1 = Payload1",
                "  { _payload1Field1 :: Text",
                "  }",
                "  deriving (Eq, Show, Generic)",
                "",
                "deriveLensAndJSON ''Payload1",
                "",
                "data Payload2 = Payload2",
                "  { _payload2Field2 :: Int",
                "  }",
                "  deriving (Eq, Show, Generic)",
                "",
                "deriveLensAndJSON ''Payload2",
                "",
                "data ExampleEmbeddedUnion",
                "  = Case1 Payload1",
                "  | Case2 Payload2",
                "  | NoPayload",
                "  deriving (Eq, Show, Generic)",
                "",
                "instance ToJSON ExampleEmbeddedUnion where",
                "  toJSON (Case1 payload) = toJSON payload & atKey \"typeTag\" ?~ String \"Case1\"",
                "  toJSON (Case2 payload) = toJSON payload & atKey \"typeTag\" ?~ String \"Case2\"",
                "  toJSON NoPayload = object [] & atKey \"typeTag\" ?~ String \"NoPayload\"",
                "",
                "instance FromJSON ExampleEmbeddedUnion where",
                "  parseJSON = withObject \"ExampleEmbeddedUnion\" $ \\o -> do",
                "    t :: Text <- o .: \"typeTag\"",
                "    case t of",
                "      \"Case1\" -> Case1 <$> parseJSON (Object o)",
                "      \"Case2\" -> Case2 <$> parseJSON (Object o)",
                "      \"NoPayload\" -> pure NoPayload",
                "      tagValue -> fail $ \"Invalid type tag: \" <> show tagValue"
              ]
      result <- parseModules ["test/examples/haskellExampleEmbeddedUnion.gotyno"]
      shouldBeRight result
      let parsedModule = result & getRight & PartialList.last
      Haskell.outputModule parsedModule `shouldBe` expectedOutput

    it "should output imports & usage of imports correctly" $ do
      let expectedOutput =
            Text.intercalate
              "\n"
              [ "{-# LANGUAGE StrictData #-}",
                "{-# LANGUAGE TemplateHaskell #-}",
                "",
                "module GotynoOutput.HaskellExampleUsingImport where",
                "",
                "import Data.Aeson (FromJSON (..), ToJSON (..))",
                "import qualified Data.Aeson as JSON",
                "import GHC.Generics (Generic)",
                "import qualified Gotyno.Helpers as Helpers",
                "import Qtility",
                "",
                "import qualified GotynoOutput.HaskellExampleStruct as HaskellExampleStruct",
                "import qualified GotynoOutput.HaskellExampleGenericStruct as HaskellExampleGenericStruct",
                "",
                "data StructUsingImport = StructUsingImport",
                "  { _structUsingImportField1 :: HaskellExampleStruct.StructOne,",
                "    _structUsingImportField2 :: (HaskellExampleGenericStruct.Holder Int Text)",
                "  }",
                "  deriving (Eq, Show, Generic)",
                "",
                "deriveLensAndJSON ''StructUsingImport",
                "",
                "data UnionUsingImport",
                "  = Case1 HaskellExampleStruct.StructOne",
                "  | Case2 (HaskellExampleGenericStruct.Holder HaskellExampleStruct.StructOne Helpers.BigInteger)",
                "  deriving (Eq, Show, Generic)",
                "",
                "instance ToJSON UnionUsingImport where",
                "  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions \"type\"",
                "",
                "instance FromJSON UnionUsingImport where",
                "  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions \"type\"",
                "",
                "makeLenses ''UnionUsingImport"
              ]
      result <-
        parseModules
          [ "test/examples/haskellExampleStruct.gotyno",
            "test/examples/haskellExampleGenericStruct.gotyno",
            "test/examples/haskellExampleUsingImport.gotyno"
          ]
      shouldBeRight result
      let parsedModule = result & getRight & PartialList.last
      Haskell.outputModule parsedModule `shouldBe` expectedOutput

    it "should output composite types correctly" $ do
      let expectedOutput =
            Text.intercalate
              "\n"
              [ "{-# LANGUAGE StrictData #-}",
                "{-# LANGUAGE TemplateHaskell #-}",
                "",
                "module GotynoOutput.HaskellExampleCompositeTypes where",
                "",
                "import Data.Aeson (FromJSON (..), ToJSON (..))",
                "import qualified Data.Aeson as JSON",
                "import GHC.Generics (Generic)",
                "import qualified Gotyno.Helpers as Helpers",
                "import Qtility",
                "",
                "data StructComposite = StructComposite",
                "  { _structCompositeField1 :: (Maybe Text),",
                "    _structCompositeField2 :: [Text],",
                "    _structCompositeField3 :: [[(Maybe Text)]],",
                "    _structCompositeField4 :: [Text],",
                "    _structCompositeField5 :: [(Maybe Text)],",
                "    _structCompositeField6 :: (Maybe [(Maybe Text)]),",
                "    _structCompositeField7 :: (Maybe [(Maybe (Maybe Text))]),",
                "    _structCompositeField8 :: (Maybe (Maybe (Maybe (Maybe Text))))",
                "  }",
                "  deriving (Eq, Show, Generic)",
                "",
                "deriveLensAndJSON ''StructComposite",
                "",
                "data UnionComposite",
                "  = Case1 (Maybe Text)",
                "  | Case2 [Text]",
                "  | Case3 [[(Maybe Text)]]",
                "  | Case4 [Text]",
                "  | Case5 [(Maybe Text)]",
                "  | Case6 (Maybe [(Maybe Text)])",
                "  | Case7 (Maybe [(Maybe (Maybe Text))])",
                "  | Case8 (Maybe (Maybe (Maybe (Maybe Text))))",
                "  deriving (Eq, Show, Generic)",
                "",
                "instance ToJSON UnionComposite where",
                "  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions \"type\"",
                "",
                "instance FromJSON UnionComposite where",
                "  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions \"type\"",
                "",
                "makeLenses ''UnionComposite"
              ]
      result <-
        parseModules ["test/examples/haskellExampleCompositeTypes.gotyno"]
      shouldBeRight result
      let parsedModule = result & getRight & PartialList.last
      Haskell.outputModule parsedModule `shouldBe` expectedOutput

getRight :: Either [String] r -> r
getRight (Right r) = r
getRight (Left e) = error $ mconcat e

shouldBeRight :: Either [String] r -> Expectation
shouldBeRight (Right _r) = pure ()
shouldBeRight (Left e) = error $ mconcat e
