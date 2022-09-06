{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GotynoOutput.Generics where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Qtility

import qualified GotynoOutput.Basic as Basic
import qualified GotynoOutput.HasGeneric as HasGeneric

data UsingGenerics = UsingGenerics
  { _usingGenericsField1 :: (Basic.Maybe' Text),
    _usingGenericsField2 :: (Basic.Either' Text Int)
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''UsingGenerics

data UsingOwnGenerics t = UsingOwnGenerics
  { _usingOwnGenericsField1 :: (Basic.Maybe' t)
  }
  deriving (Eq, Show, Generic)

instance (FromJSON t) => FromJSON (UsingOwnGenerics t) where
  parseJSON = JSON.genericParseJSON
    JSON.defaultOptions {JSON.fieldLabelModifier = drop @[] (length "_UsingOwnGenerics") >>> lowerCaseFirst}

instance (ToJSON t) => ToJSON (UsingOwnGenerics t) where
  toJSON = JSON.genericToJSON
    JSON.defaultOptions {JSON.fieldLabelModifier = drop @[] (length "_UsingOwnGenerics") >>> lowerCaseFirst}

makeLenses ''UsingOwnGenerics

data KnownForMovie = KnownForMovie
  { _knownForMovieMedia_type :: Text,
    _knownForMoviePoster_path :: (Maybe Text),
    _knownForMovieId :: Int,
    _knownForMovieTitle :: (Maybe Text),
    _knownForMovieVote_average :: Float,
    _knownForMovieRelease_date :: (Maybe Text),
    _knownForMovieOverview :: Text
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''KnownForMovie

data KnownForShow = KnownForShow
  { _knownForShowMedia_type :: Text,
    _knownForShowPoster_path :: (Maybe Text),
    _knownForShowId :: Int,
    _knownForShowVote_average :: Float,
    _knownForShowOverview :: Text,
    _knownForShowFirst_air_date :: (Maybe Text),
    _knownForShowName :: (Maybe Text)
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''KnownForShow

data KnownFor
  = KnownForKnownForShow KnownForShow
  | KnownForKnownForMovie KnownForMovie
  | KnownForString Text
  | KnownForF32 Float
  deriving (Eq, Show, Generic)

instance FromJSON KnownFor where
  parseJSON v =
    (KnownForKnownForShow <$> parseJSON v)
      <|> (KnownForKnownForMovie <$> parseJSON v)
      <|> (KnownForString <$> parseJSON v)
      <|> (KnownForF32 <$> parseJSON v)

instance ToJSON KnownFor where
  toJSON (KnownForKnownForShow v) = toJSON v
  toJSON (KnownForKnownForMovie v) = toJSON v
  toJSON (KnownForString v) = toJSON v
  toJSON (KnownForF32 v) = toJSON v

data KnownForMovieWithoutTypeTag = KnownForMovieWithoutTypeTag
  { _knownForMovieWithoutTypeTagPoster_path :: (Maybe Text),
    _knownForMovieWithoutTypeTagId :: Int,
    _knownForMovieWithoutTypeTagTitle :: (Maybe Text),
    _knownForMovieWithoutTypeTagVote_average :: Float,
    _knownForMovieWithoutTypeTagRelease_date :: (Maybe Text),
    _knownForMovieWithoutTypeTagOverview :: Text
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''KnownForMovieWithoutTypeTag

data KnownForShowWithoutTypeTag = KnownForShowWithoutTypeTag
  { _knownForShowWithoutTypeTagPoster_path :: (Maybe Text),
    _knownForShowWithoutTypeTagId :: Int,
    _knownForShowWithoutTypeTagVote_average :: Float,
    _knownForShowWithoutTypeTagOverview :: Text,
    _knownForShowWithoutTypeTagFirst_air_date :: (Maybe Text),
    _knownForShowWithoutTypeTagName :: (Maybe Text)
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''KnownForShowWithoutTypeTag

data KnownForEmbedded
  = MovieStartingWithLowercase KnownForMovieWithoutTypeTag
  | TvStartingWithLowercase KnownForShowWithoutTypeTag
  deriving (Eq, Show, Generic)

instance ToJSON KnownForEmbedded where
  toJSON (MovieStartingWithLowercase payload) = toJSON payload & atKey "media_type" ?~ String "movieStartingWithLowercase"
  toJSON (TvStartingWithLowercase payload) = toJSON payload & atKey "media_type" ?~ String "tvStartingWithLowercase"

instance FromJSON KnownForEmbedded where
  parseJSON = withObject "KnownForEmbedded" $ \o -> do
    t :: Text <- o .: "media_type"
    case t of
      "movieStartingWithLowercase" -> MovieStartingWithLowercase <$> parseJSON (Object o)
      "tvStartingWithLowercase" -> TvStartingWithLowercase <$> parseJSON (Object o)
      tagValue -> fail $ "Invalid type tag: " <> show tagValue

data KnownForEmbeddedWithUpperCase
  = Movie KnownForMovieWithoutTypeTag
  | Tv KnownForShowWithoutTypeTag
  deriving (Eq, Show, Generic)

instance ToJSON KnownForEmbeddedWithUpperCase where
  toJSON (Movie payload) = toJSON payload & atKey "media_type" ?~ String "Movie"
  toJSON (Tv payload) = toJSON payload & atKey "media_type" ?~ String "Tv"

instance FromJSON KnownForEmbeddedWithUpperCase where
  parseJSON = withObject "KnownForEmbeddedWithUpperCase" $ \o -> do
    t :: Text <- o .: "media_type"
    case t of
      "Movie" -> Movie <$> parseJSON (Object o)
      "Tv" -> Tv <$> parseJSON (Object o)
      tagValue -> fail $ "Invalid type tag: " <> show tagValue
