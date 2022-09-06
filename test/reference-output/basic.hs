{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GotynoOutput.Basic where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Qtility

data Recruiter = Recruiter
  { _recruiterType :: Text,
    _recruiterName :: Text,
    _recruiterEmails :: [(Maybe Text)],
    _recruiterRecruiter :: (Maybe Recruiter),
    _recruiterCreated :: Helpers.BigInteger
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''Recruiter

data GetSearchesFilter
  = SearchesByQueryLike Text
  | SearchesByResultLike Text
  | NoSearchesFilter
  deriving (Eq, Show, Generic)

instance ToJSON GetSearchesFilter where
  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions "type"

instance FromJSON GetSearchesFilter where
  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions "type"

data SearchesParameters = SearchesParameters
  { _searchesParametersFilters :: [GetSearchesFilter]
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''SearchesParameters

data StillSize
  = StillSizeW92
  | StillSizeW185
  | StillSizeW300
  | StillSizeH632
  | StillSizeOriginal
  deriving (Eq, Show, Generic)

instance ToJSON StillSize where
  toJSON StillSizeW92 = String "w92"
  toJSON StillSizeW185 = String "w185"
  toJSON StillSizeW300 = String "w300"
  toJSON StillSizeH632 = String "h632"
  toJSON StillSizeOriginal = String "original"

instance FromJSON StillSize where
  parseJSON = Helpers.enumFromJSON [(String "w92", StillSizeW92), (String "w185", StillSizeW185), (String "w300", StillSizeW300), (String "h632", StillSizeH632), (String "original", StillSizeOriginal)]

data LogInData = LogInData
  { _logInDataUsername :: Text,
    _logInDataPassword :: Text
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''LogInData

data UserId = UserId
  { _userIdValue :: Text
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''UserId

data Channel = Channel
  { _channelName :: Text,
    _channelPrivate :: Bool
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''Channel

data Email = Email
  { _emailValue :: Text,
    _emailPublic :: Bool
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''Email

data Event
  = LogIn LogInData
  | LogOut UserId
  | JoinChannels [Channel]
  | SetEmails [Email]
  deriving (Eq, Show, Generic)

instance ToJSON Event where
  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions "type"

instance FromJSON Event where
  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions "type"

data Maybe' t
  = Nothing'
  | Just' t
  deriving (Eq, Show, Generic)

instance (ToJSON t) => ToJSON (Maybe' t) where
  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions "type"

instance (FromJSON t) => FromJSON (Maybe' t) where
  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions "type"

data Either' l r
  = Left' l
  | Right' r
  deriving (Eq, Show, Generic)

instance (ToJSON l, ToJSON r) => ToJSON (Either' l r) where
  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions "type"

instance (FromJSON l, FromJSON r) => FromJSON (Either' l r) where
  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions "type"

data Person = Person
  { _personName :: Text,
    _personAge :: Int,
    _personEfficiency :: Float,
    _personOn_vacation :: Bool,
    _personHobbies :: [Text],
    _personLast_fifteen_comments :: [Text],
    _personRecruiter :: Recruiter,
    _personSpouse :: (Maybe Person)
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''Person

data EmbeddedEvent
  = EmbeddedLogIn LogInData
  | SystemImploded
  deriving (Eq, Show, Generic)

instance ToJSON EmbeddedEvent where
  toJSON (EmbeddedLogIn payload) = toJSON payload & atKey "type" ?~ String "EmbeddedLogIn"
  toJSON SystemImploded = object [] & atKey "type" ?~ String "SystemImploded"

instance FromJSON EmbeddedEvent where
  parseJSON = withObject "EmbeddedEvent" $ \o -> do
    t :: Text <- o .: "type"
    case t of
      "EmbeddedLogIn" -> EmbeddedLogIn <$> parseJSON (Object o)
      "SystemImploded" -> pure SystemImploded
      tagValue -> fail $ "Invalid type tag: " <> show tagValue
