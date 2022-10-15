{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GotynoOutput.BasicOptional where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Qtility

data HasOptionalString = HasOptionalString
  { _hasOptionalStringStringField :: (Maybe Text),
    _hasOptionalStringOptionalArrayField :: (Maybe [Int]),
    _hasOptionalStringArrayOfOptionalField :: [(Maybe Int)]
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''HasOptionalString

data HasOptionalConstructor
  = DoesNot Int
  | Does (Maybe Int)
  | HasOptionalStruct (Maybe HasOptionalString)
  deriving (Eq, Show, Generic)

instance ToJSON HasOptionalConstructor where
  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions "type"

instance FromJSON HasOptionalConstructor where
  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions "type"
