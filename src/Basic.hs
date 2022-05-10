module Basic where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Prelude

data Recruiter = Recruiter
  { recruiterType :: !String,
    recruiterName :: !String,
    recruiterEmails :: ![Maybe String],
    recruiterRecruiter :: !(Maybe Recruiter),
    recruiterCreated :: !Integer
  }
  deriving (Eq, Show, Generic)

instance FromJSON Recruiter where
  parseJSON = JSON.withObject "Recruiter" $ \o -> do
    recruiterType <- o .: "type" >>= Helpers.checkEqualTo "Recruiter"
    recruiterName <- o .: "Name"
    recruiterEmails <- o .: "emails"
    recruiterRecruiter <- o .:? "recruiter"
    Helpers.BigInteger recruiterCreated <- o .: "created"
    pure $
      Recruiter
        { recruiterType,
          recruiterName,
          recruiterEmails,
          recruiterRecruiter,
          recruiterCreated
        }

instance ToJSON Recruiter where
  toJSON value =
    JSON.object
      [ "type" .= Helpers.LiteralString "Recruiter",
        "name" .= recruiterName value,
        "emails" .= recruiterEmails value,
        "recruiter" .= recruiterRecruiter value,
        "created" .= Helpers.BigInteger (recruiterCreated value)
      ]

-- struct Recruiter {
--     type: "Recruiter"
--     Name: String
--     emails: [3]?String
--     recruiter: ?*Recruiter
--     created: U64
-- }
