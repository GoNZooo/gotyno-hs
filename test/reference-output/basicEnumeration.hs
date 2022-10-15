{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GotynoOutput.BasicEnumeration where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Qtility

data StringValues
  = StringValuesFirst
  | StringValuesSecond
  | StringValuesThird
  | StringValuesFourth
  deriving (Eq, Show, Generic)

instance ToJSON StringValues where
  toJSON StringValuesFirst = String "first"
  toJSON StringValuesSecond = String "second"
  toJSON StringValuesThird = String "Third"
  toJSON StringValuesFourth = String "Fourth"

instance FromJSON StringValues where
  parseJSON = Helpers.enumFromJSON [(String "first", StringValuesFirst), (String "second", StringValuesSecond), (String "Third", StringValuesThird), (String "Fourth", StringValuesFourth)]

data IntValues
  = IntValuesFirst
  | IntValuesSecond
  | IntValuesThird
  | IntValuesFourth
  deriving (Eq, Show, Generic)

instance ToJSON IntValues where
  toJSON IntValuesFirst = Number $ fromInteger 1
  toJSON IntValuesSecond = Number $ fromInteger 2
  toJSON IntValuesThird = Number $ fromInteger 3
  toJSON IntValuesFourth = Number $ fromInteger 4

instance FromJSON IntValues where
  parseJSON = Helpers.enumFromJSON [(Number $ fromInteger 1, IntValuesFirst), (Number $ fromInteger 2, IntValuesSecond), (Number $ fromInteger 3, IntValuesThird), (Number $ fromInteger 4, IntValuesFourth)]
