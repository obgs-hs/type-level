module Main where

import Test.Hspec
import Obgs.Fcf.EnumSpecs (typeLevelEnumSpecs)
import Obgs.TL.IntegralSpecs (typeLevelIntegralSpecs)
import Obgs.TL.FinitarySpecs (typeLevelFinitarySpecs)

main :: IO ()
main = hspec $ do
  describe "Obgs.Fcf.Enum" typeLevelEnumSpecs
  describe "Obgs.TL.Finitary" typeLevelFinitarySpecs
  describe "Obgs.TL.Integral" typeLevelIntegralSpecs