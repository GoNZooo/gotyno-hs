module Main where

import qualified HaskellOutputSpec
import qualified ParsingSpec
import RIO
import Test.Hspec

main :: IO ()
main = do
  tsReferenceOutput <- ParsingSpec.typeScriptReferenceOutput
  fsReferenceOutput <- ParsingSpec.fSharpReferenceOutput
  pyReferenceOutput <- ParsingSpec.pythonReferenceOutput
  hspec $ do
    ParsingSpec.spec tsReferenceOutput fsReferenceOutput pyReferenceOutput
    HaskellOutputSpec.spec
