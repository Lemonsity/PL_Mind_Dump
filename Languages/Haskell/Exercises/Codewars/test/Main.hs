module Main where

import CamelCaseSpec
import MultiplesOf3And5Spec
import Test.Hspec

main :: IO ()
main = hspec Main.spec

spec :: SpecWith ()
spec = do
  describe "CamelCase" CamelCaseSpec.spec
  describe "MultiplesOf3And5" MultiplesOf3And5Spec.spec
