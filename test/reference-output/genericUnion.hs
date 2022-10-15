{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GotynoOutput.GenericUnion where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Qtility

data GenericUnion t
  = HasTPayload t
  | HasNoPayload
  deriving (Eq, Show, Generic)

instance (ToJSON t) => ToJSON (GenericUnion t) where
  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions "type"

instance (FromJSON t) => FromJSON (GenericUnion t) where
  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions "type"
