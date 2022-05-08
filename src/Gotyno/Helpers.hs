module Gotyno.Helpers where

import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Qtility
import qualified RIO.Text as Text

-- | Used for a more explicit style in `toJSON` instances. It also means we don't have to add type
-- annotations after the value.
newtype LiteralString = LiteralString Text
  deriving (Eq, Show, ToJSON)

-- | Used to encode `Integer` ({I,U}{64,128}) values, because there are ecosystems where these
-- cannot be decoded properly without having them come in as strings in transit.
newtype BigInteger = BigInteger Integer
  deriving (Eq, Show)

instance FromJSON BigInteger where
  parseJSON = JSON.withText "BigInteger" $ \text ->
    case text & Text.unpack & readMaybe of
      Just i -> pure $ BigInteger i
      Nothing -> fail $ mconcat ["Expected value readable as bigint, got: ", show text]

instance ToJSON BigInteger where
  toJSON (BigInteger i) = JSON.String $ tshow i

-- | Checks that a value matches an expectation, used to check literals.
checkEqualTo :: (Eq a, Show a) => a -> a -> Parser a
checkEqualTo expected actual
  | expected == actual = pure actual
  | otherwise = fail $ "Expected: " <> show expected <> " but got: " <> show actual

gotynoOptions :: String -> AesonOptions
gotynoOptions typeTag =
  defaultAesonOptions
    & sumEncoding .~ TaggedObject {tagFieldName = typeTag, contentsFieldName = "data"}
