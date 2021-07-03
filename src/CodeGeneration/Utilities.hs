module CodeGeneration.Utilities where

import RIO
import qualified RIO.Char as Char
import qualified RIO.Text as Text

upperCaseFirstCharacter :: Text -> Text
upperCaseFirstCharacter t =
  case Text.uncons t of
    Just (c, rest) -> Text.cons (Char.toUpper c) rest
    Nothing -> t
