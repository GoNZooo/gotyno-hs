{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module GotynoOutput.Github where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Qtility

data UserData = UserData
  { _userDataLogin :: Text,
    _userDataId :: Int,
    _userDataAvatar_url :: Text,
    _userDataUrl :: Text,
    _userDataHtml_url :: Text,
    _userDataFollowers_url :: Text,
    _userDataGists_url :: Text,
    _userDataRepos_url :: Text,
    _userDataSite_admin :: Bool,
    _userDataBio :: Text,
    _userDataPublic_repos :: Int,
    _userDataFollowers :: Int,
    _userDataFollowing :: Int,
    _userDataCreated_at :: Text,
    _userDataUpdated_at :: Text,
    _userDataLocation :: (Maybe Text),
    _userDataBlog :: (Maybe Text)
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''UserData

data OwnerData = OwnerData
  { _ownerDataId :: Int,
    _ownerDataLogin :: Text,
    _ownerDataUrl :: Text,
    _ownerDataHtml_url :: Text,
    _ownerDataFollowers_url :: Text,
    _ownerDataGists_url :: Text,
    _ownerDataRepos_url :: Text,
    _ownerDataSite_admin :: Bool
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''OwnerData

data OrganizationData = OrganizationData
  { _organizationDataLogin :: Text,
    _organizationDataId :: Int,
    _organizationDataAvatar_url :: Text,
    _organizationDataMembers_url :: (Maybe Text),
    _organizationDataRepos_url :: Text,
    _organizationDataDescription :: (Maybe Text)
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''OrganizationData

data Owner
  = User OwnerData
  | Organization OrganizationData
  deriving (Eq, Show, Generic)

instance ToJSON Owner where
  toJSON (User payload) = toJSON payload & atKey "type" ?~ String "User"
  toJSON (Organization payload) = toJSON payload & atKey "type" ?~ String "Organization"

instance FromJSON Owner where
  parseJSON = withObject "Owner" $ \o -> do
    t :: Text <- o .: "type"
    case t of
      "User" -> User <$> parseJSON (Object o)
      "Organization" -> Organization <$> parseJSON (Object o)
      tagValue -> fail $ "Invalid type tag: " <> show tagValue

data Repository = Repository
  { _repositoryId :: Int,
    _repositoryName :: Text,
    _repositoryFull_name :: Text,
    _repositoryPrivate :: Bool,
    _repositoryFork :: Bool,
    _repositoryCreated_at :: Text,
    _repositoryUpdated_at :: Text,
    _repositoryDescription :: (Maybe Text),
    _repositoryOwner :: Owner,
    _repositoryUrl :: Text,
    _repositoryHtml_url :: Text,
    _repositoryLanguage :: (Maybe Text)
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''Repository

data Pusher = Pusher
  { _pusherName :: Text,
    _pusherEmail :: Text
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''Pusher

data Author = Author
  { _authorName :: Text,
    _authorEmail :: Text,
    _authorUsername :: Text
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''Author

data Label = Label
  { _labelId :: Int,
    _labelUrl :: Text,
    _labelName :: Text,
    _labelColor :: Text,
    _labelDefault :: Bool,
    _labelDescription :: Text
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''Label

data Issue = Issue
  { _issueId :: Int,
    _issueUrl :: Text,
    _issueHtml_url :: Text,
    _issueRepository_url :: Text,
    _issueNumber :: Int,
    _issueTitle :: Text,
    _issueUser :: UserData,
    _issueLabels :: [Label],
    _issueState :: Text,
    _issueLocked :: Bool,
    _issueAssignee :: (Maybe UserData),
    _issueAssignees :: [UserData],
    _issueComments :: Int,
    _issueCreated_at :: Text,
    _issueUpdated_at :: Text,
    _issueClosed_at :: (Maybe Text),
    _issueAuthor_association :: Text,
    _issueBody :: Text
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''Issue

data Commit = Commit
  { _commitId :: Text,
    _commitTree_id :: Text,
    _commitDistinct :: Bool,
    _commitMessage :: Text,
    _commitTimestamp :: Text,
    _commitUrl :: Text,
    _commitAuthor :: Author,
    _commitCommitter :: Author,
    _commitAdded :: [Text],
    _commitRemoved :: [Text],
    _commitModified :: [Text]
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''Commit

data PushData = PushData
  { _pushDataRepository :: Repository,
    _pushDataRef :: Text,
    _pushDataBefore :: Text,
    _pushDataAfter :: Text,
    _pushDataPusher :: Pusher,
    _pushDataOrganization :: OrganizationData,
    _pushDataSender :: UserData,
    _pushDataCreated :: Bool,
    _pushDataDeleted :: Bool,
    _pushDataForced :: Bool,
    _pushDataCompare :: Text,
    _pushDataCommits :: [Commit],
    _pushDataHead_commit :: Commit
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''PushData

data WebhookEvent
  = Push PushData
  deriving (Eq, Show, Generic)

instance ToJSON WebhookEvent where
  toJSON = JSON.genericToJSON $ Helpers.gotynoOptions "type"

instance FromJSON WebhookEvent where
  parseJSON = JSON.genericParseJSON $ Helpers.gotynoOptions "type"

data RepositorySearchData = RepositorySearchData
  { _repositorySearchDataTotal_count :: Int,
    _repositorySearchDataIncomplete_results :: Bool,
    _repositorySearchDataItems :: [Repository]
  }
  deriving (Eq, Show, Generic)

deriveLensAndJSON ''RepositorySearchData
