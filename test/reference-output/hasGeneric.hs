{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GotynoOutput.HasGeneric where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Qtility

import qualified GotynoDeclarations.External as External
import qualified GotynoDeclarations.Other as Other

data Result t e
  = Success t
  | Failure e
  deriving (Eq, Show, Generic)

instance (ToJSON t, ToJSON e) => ToJSON (Result t e) where
  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions "type"

instance (FromJSON t, FromJSON e) => FromJSON (Result t e) where
  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions "type"

data Holder t = Holder
  { _holderValue :: t
  }
  deriving (Eq, Show, Generic)

instance (FromJSON t) => FromJSON (Holder t) where
  parseJSON = JSON.genericParseJSON
    JSON.defaultOptions {JSON.fieldLabelModifier = drop @[] (length "_Holder") >>> lowerCaseFirst}

instance (ToJSON t) => ToJSON (Holder t) where
  toJSON = JSON.genericToJSON
    JSON.defaultOptions {JSON.fieldLabelModifier = drop @[] (length "_Holder") >>> lowerCaseFirst}

makeLenses ''Holder

data MaybeHolder t = MaybeHolder
  { _maybeHolderValue :: (External.Option t),
    _maybeHolderOtherValue :: Other.Plain
  }
  deriving (Eq, Show, Generic)

instance (FromJSON t) => FromJSON (MaybeHolder t) where
  parseJSON = JSON.genericParseJSON
    JSON.defaultOptions {JSON.fieldLabelModifier = drop @[] (length "_MaybeHolder") >>> lowerCaseFirst}

instance (ToJSON t) => ToJSON (MaybeHolder t) where
  toJSON = JSON.genericToJSON
    JSON.defaultOptions {JSON.fieldLabelModifier = drop @[] (length "_MaybeHolder") >>> lowerCaseFirst}

makeLenses ''MaybeHolder

data HasGenericEvent t
  = PlainEvent Other.Plain
  | GenericEvent (External.Option t)
  deriving (Eq, Show, Generic)

instance (ToJSON t) => ToJSON (HasGenericEvent t) where
  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions "type"

instance (FromJSON t) => FromJSON (HasGenericEvent t) where
  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions "type"
