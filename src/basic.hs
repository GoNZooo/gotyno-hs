module Basic where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import Prelude (($))
import qualified Prelude

data Recruiter = Recruiter
  { recruiterType :: !Prelude.String,
    recruiterName :: !Prelude.String,
    recruiterEmails :: ![Prelude.Maybe Prelude.String],
    recruiterRecruiter :: !(Prelude.Maybe Recruiter),
    recruiterCreated :: !Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Show, Generic)

instance FromJSON Recruiter where
  parseJSON = JSON.withObject "Recruiter" $ \o -> do
    recruiterType <- Helpers.readLiteralString o "type" "Recruiter"
    recruiterName <- o .: "name"
    recruiterEmails <- o .: "emails"
    recruiterRecruiter <- o .:? "recruiter"
    recruiterCreated <- o .: "created"
    Prelude.pure $
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
      [ "type" .= recruiterType value,
        "name" .= recruiterName value,
        "emails" .= recruiterEmails value,
        "recruiter" .= recruiterRecruiter value,
        "created" .= recruiterCreated value
      ]

-- struct Recruiter {
--     type: "Recruiter"
--     Name: String
--     emails: [3]?String
--     recruiter: ?*Recruiter
--     created: U64
-- }
