module Gotyno.Helpers where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:))
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import RIO
import qualified RIO.Text as Text

newtype StringEncodedInteger = StringEncodedInteger Integer
  deriving (Eq, Show)

instance FromJSON StringEncodedInteger where
  parseJSON = JSON.withText "StringEncodedInteger" $ \text ->
    case text & Text.unpack & readMaybe of
      Just i -> pure $ StringEncodedInteger i
      Nothing -> fail $ mconcat ["Expected value readable as bigint, got: ", show text]

instance ToJSON StringEncodedInteger where
  toJSON (StringEncodedInteger i) = JSON.String $ tshow i

-- | Reads a value from a given key and expects it to match a given value.
parseLiteralString :: (Eq a, Show a, FromJSON a) => JSON.Object -> Text -> a -> Parser a
parseLiteralString value key soughtValue = do
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

parseBigInt :: JSON.Object -> Text -> Parser Integer
parseBigInt value key = do
  readValue <- value .: key
  case readMaybe readValue of
    Just integerValue -> pure integerValue
    Nothing -> fail $ mconcat ["Expected value readable as bigint, got: ", show readValue]
