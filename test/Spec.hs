module Main where

import qualified ParsingSpec
import RIO
import Test.Hspec

main :: IO ()
main = do
  basicTypeScriptOutput <- ParsingSpec.basicTypeScriptReferenceOutput
  importExampleTypeScriptOutput <- ParsingSpec.importTypeScriptReferenceOutput
  hasGenericTypeScriptOutput <- ParsingSpec.hasGenericTypeScriptReferenceOutput
  genericsTypeScriptOutput <- ParsingSpec.genericsTypeScriptReferenceOutput
  hspec $
    ParsingSpec.spec
      basicTypeScriptOutput
      importExampleTypeScriptOutput
      hasGenericTypeScriptOutput
      genericsTypeScriptOutput
