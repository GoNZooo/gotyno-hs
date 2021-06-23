module Main where

import qualified ParsingSpec
import RIO
import Test.Hspec

main :: IO ()
main = do
  basicTypeScriptOutput <- ParsingSpec.basicTypeScriptReferenceOutput
  hspec $ ParsingSpec.spec basicTypeScriptOutput
