{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GotynoOutput.BasicUnion where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Qtility

data PayloadStruct = PayloadStruct
  { _payloadStructField1 :: Int
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''PayloadStruct

data BasicUnion
  = HasStringPayload Text
  | HasPayload PayloadStruct
  | HasNoPayload
  deriving (Eq, Show, Generic)

instance ToJSON BasicUnion where
  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions "type"

instance FromJSON BasicUnion where
  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions "type"
