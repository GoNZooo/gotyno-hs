module CodeGeneration.TypeScript where

import RIO
import qualified RIO.Text as Text
import Types

outputModule :: Module -> Text
outputModule _module =
  Text.unlines [modulePrelude, ""]

modulePrelude :: Text
modulePrelude = "import * as svt from \"simple-validation-tools\";"
