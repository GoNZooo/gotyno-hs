module Basic where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import qualified Gotyno.Helpers as Helpers
import qualified Prelude

data Recruiter = Recruiter
  { recruiterType :: String,
    recruiterName :: String,
    recruiterEmails :: [Maybe String],
    recruiterRecruiter :: Maybe Recruiter,
    recruiterCreated :: Integer
  }
  deriving (Eq, Show, Generic)

instance FromJSON Recruiter where
  fromJSON = JSON.withObject "Recruiter" $ \o -> do
    recruiterType <- o .: "type"
    _x

-- struct Recruiter {
--     type: "Recruiter"
--     Name: String
--     emails: [3]?String
--     recruiter: ?*Recruiter
--     created: U64
-- }
