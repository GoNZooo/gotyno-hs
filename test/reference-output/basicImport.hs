{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GotynoOutput.BasicImport where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Qtility

import qualified GotynoOutput.BasicStruct as BasicStruct

data StructUsingImport = StructUsingImport
  { _structUsingImportField :: BasicStruct.BasicStruct
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''StructUsingImport

data UnionUsingImport
  = ConstructorWithPayload BasicStruct.BasicStruct
  deriving (Eq, Show, Generic)

instance ToJSON UnionUsingImport where
  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions "type"

instance FromJSON UnionUsingImport where
  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions "type"
