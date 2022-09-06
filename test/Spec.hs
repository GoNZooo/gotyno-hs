module Main where

import qualified HaskellOutputSpec
import qualified ParsingSpec
import Qtility
import Test.Hspec

main :: IO ()
main = do
  tsReferenceOutput <- ParsingSpec.typeScriptReferenceOutput
  hsReferenceOutput <- ParsingSpec.haskellReferenceOutput
  fsReferenceOutput <- ParsingSpec.fSharpReferenceOutput
  pyReferenceOutput <- ParsingSpec.pythonReferenceOutput
  ktReferenceOutput <- ParsingSpec.kotlinReferenceOutput
  hspec $ do
    ParsingSpec.spec
      tsReferenceOutput
      hsReferenceOutput
      fsReferenceOutput
      pyReferenceOutput
      ktReferenceOutput
    HaskellOutputSpec.spec
