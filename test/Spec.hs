module Main where

import qualified ParsingSpec
import RIO
import Test.Hspec

main :: IO ()
main = do
  basicTypeScriptOutput <- ParsingSpec.basicTypeScriptReferenceOutput
  importExampleTypeScriptOutput <- ParsingSpec.importTypeScriptReferenceOutput
  hasGenericTypeScriptOutput <- ParsingSpec.hasGenericTypeScriptReferenceOutput
  hspec $
    ParsingSpec.spec
      basicTypeScriptOutput
      importExampleTypeScriptOutput
      hasGenericTypeScriptOutput
