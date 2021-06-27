module Main where

import qualified ParsingSpec
import RIO
import Test.Hspec

main :: IO ()
main = do
  tsReferenceOutput <- ParsingSpec.typeScriptReferenceOutput
  fsReferenceOutput <- ParsingSpec.fSharpReferenceOutput
  hspec $ ParsingSpec.spec tsReferenceOutput fsReferenceOutput
