module Gotyno.Helpers where

import Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Prelude

-- | Reads a value from a given key and expects it to match a given value.
readLiteralString :: (Eq a, Show a, FromJSON a) => JSON.Object -> Text -> a -> Parser a
readLiteralString value key soughtValue = do
  readValue <- value .: key
  if readValue == soughtValue
    then pure readValue
    else
      fail $
        mconcat
          [ "Expected value '",
            show soughtValue,
            "' but got '",
            show readValue,
            "'"
          ]
