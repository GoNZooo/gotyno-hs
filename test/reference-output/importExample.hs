{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GotynoOutput.ImportExample where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Qtility

import qualified GotynoOutput.Basic as Basic

data UsesImport = UsesImport
  { _usesImportType :: Text,
    _usesImportRecruiter :: Basic.Recruiter
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''UsesImport

data HoldsSomething t = HoldsSomething
  { _holdsSomethingHoldingField :: t
  }
  deriving (Eq, Show, Generic)

instance (FromJSON t) => FromJSON (HoldsSomething t) where
  parseJSON =
    JSON.genericParseJSON
      JSON.defaultOptions
        {JSON.fieldLabelModifier = drop @[] (length "_HoldsSomething") >>> lowerCaseFirst}

instance (ToJSON t) => ToJSON (HoldsSomething t) where
  toJSON =
    JSON.genericToJSON
      JSON.defaultOptions
        {JSON.fieldLabelModifier = drop @[] (length "_HoldsSomething") >>> lowerCaseFirst}

makeLenses ''HoldsSomething

data StructureUsingImport = StructureUsingImport
  { _structureUsingImportEvent :: Basic.Event
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''StructureUsingImport

data UnionUsingImport
  = CoolEvent Basic.Event
  | Other Basic.Person
  deriving (Eq, Show, Generic)

instance ToJSON UnionUsingImport where
  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions "type"

instance FromJSON UnionUsingImport where
  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions "type"

data AllConcrete = AllConcrete
  { _allConcreteField :: (HoldsSomething (Basic.Either' (Basic.Maybe' StructureUsingImport) UnionUsingImport))
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''AllConcrete
