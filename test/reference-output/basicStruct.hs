{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GotynoOutput.BasicStruct where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Qtility

data BasicStruct = BasicStruct
  { _basicStructField1 :: Int,
    _basicStructField2 :: Text
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''BasicStruct
