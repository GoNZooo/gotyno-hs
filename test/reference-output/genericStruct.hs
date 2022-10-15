{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GotynoOutput.GenericStruct where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Qtility

data GenericStruct t = GenericStruct
  { _genericStructField :: t
  }
  deriving (Eq, Show, Generic)

instance (FromJSON t) => FromJSON (GenericStruct t) where
  parseJSON =
    JSON.genericParseJSON
      JSON.defaultOptions
        {JSON.fieldLabelModifier = drop @[] (length "_GenericStruct") >>> lowerCaseFirst}

instance (ToJSON t) => ToJSON (GenericStruct t) where
  toJSON =
    JSON.genericToJSON
      JSON.defaultOptions
        {JSON.fieldLabelModifier = drop @[] (length "_GenericStruct") >>> lowerCaseFirst}

makeLenses ''GenericStruct
