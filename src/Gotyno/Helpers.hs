module Gotyno.Helpers where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:))
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import RIO
import qualified RIO.Text as Text

-- | Used for a more explicit style in `toJSON` instances. It also means we don't have to add type
-- annotations after the value.
newtype LiteralString = LiteralString Text
  deriving (Eq, Show, ToJSON)

-- | Used to encode `Integer` ({I,U}{64,128}) values, because there are ecosystems where these
-- cannot be decoded properly without having them come in as strings in transit.
newtype StringEncodedInteger = StringEncodedInteger Integer
  deriving (Eq, Show)

instance FromJSON StringEncodedInteger where
  parseJSON = JSON.withText "StringEncodedInteger" $ \text ->
    case text & Text.unpack & readMaybe of
      Just i -> pure $ StringEncodedInteger i
      Nothing -> fail $ mconcat ["Expected value readable as bigint, got: ", show text]

instance ToJSON StringEncodedInteger where
  toJSON (StringEncodedInteger i) = JSON.String $ tshow i

-- | Checks that a value matches an expectation, used to check literals.
checkEqualTo :: (Eq a, Show a) => a -> a -> Parser a
checkEqualTo expected actual
  | expected == actual = pure actual
  | otherwise = fail $ "Expected: " <> show expected <> " but got: " <> show actual

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
