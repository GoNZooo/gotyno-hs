module HaskellOutputSpec where

import qualified CodeGeneration.Haskell as Haskell
import Parsing
import RIO
import RIO.Directory (getCurrentDirectory)
import qualified RIO.Text as Text
import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "`outputModule`" $ do
    it "should output a basic struct" $ do
      let expectedOutput =
            Text.unlines
              [ "{-# LANGUAGE StrictData #-}",
                "{-# LANGUAGE TemplateHaskell #-}",
                "",
                "module GotynoOutput.HaskellExampleStruct where",
                "",
                "import Data.Aeson (FromJSON (..), ToJSON (..))",
                "import qualified Data.Aeson as JSON",
                "import GHC.Generics (Generic)",
                "import qualified Gotyno.Helpers as Helpers",
                "import qualified Prelude",
                "",
                "data StructOne = StructOne",
                "  { _structOneUnsigned_integer_field :: Int,",
                "    _structOneStringField :: Text,",
                "    _structOneBig_integer_field :: Helpers.BigInteger",
                "  }"
              ]
      currentDirectory <- getCurrentDirectory
      Right [parsedModule] <- parseModules ["test/examples/haskellExampleStruct.gotyno"]
      Haskell.outputModule parsedModule `shouldBe` expectedOutput

getRight :: Either [String] r -> r
getRight (Right r) = r
getRight (Left e) = error $ mconcat e

shouldBeRight :: Either [String] r -> Expectation
shouldBeRight (Right _r) = pure ()
shouldBeRight (Left e) = error $ mconcat e
