{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -freduction-depth=0 #-}
{-# OPTIONS -Wno-missing-signatures #-}

module Obgs.Fcf.EnumSpecs where

import Data.Proxy (Proxy (..))
import Obgs.Fcf.Enum qualified as Enum
import Test.Hspec

typeLevelEnumSpecs :: SpecWith ()
typeLevelEnumSpecs = do
  describe "Provides valid 'PBounded' and 'PEnum' instances for:" $ do
    describe "Bool" $ testBoundedInstance (Proxy @Bool)
    describe "Ordering" $ testBoundedInstance (Proxy @Ordering)

  describe "Provides a valid 'PEnum' instance for:" $ do
    describe "Ordering" $ testEnumInstance (Proxy @'( 'EQ, 'GT, 'GT))
  where
    testEnumInstance = Enum.testEnumInstance specify shouldBe
    testBoundedInstance = Enum.testBoundedEnumInstance specify shouldBe
