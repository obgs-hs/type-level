{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -freduction-depth=0 #-}
{-# OPTIONS -Wno-missing-signatures #-}

module Obgs.TL.FinitarySpecs where

import Data.Int (Int8)
import Data.Finite (Finite)
import Data.Proxy (Proxy (..))
import Obgs.Fcf.Finitary
import Obgs.TL.Integral (TypeIntegral)
import Test.Hspec

typeLevelFinitarySpecs :: SpecWith ()
typeLevelFinitarySpecs = do
  describe "Provides valid 'PFinitary' instances for:" $ do
    describe "()" $ testSmallInstance (Proxy @())
    describe "Bool" $ testSmallInstance (Proxy @Bool)
    describe "Char" $ testBigInstance (Proxy @Char)
    describe "(,)" $ testSmallInstance (Proxy @(Bool, SmallFinite))
    describe "Maybe" $ testSmallInstance (Proxy @(Maybe SmallFinite))
    describe "TypeIntegral Int8" $ testSmallInstance (Proxy @(TypeIntegral Int8))
    describe "TypeIntegral (Finite 10)" $ testSmallInstance (Proxy @SmallFinite)
  where
    testBigInstance = testPFinitaryInstance specify shouldBe
    testSmallInstance = testPFinitarySmallInstance specify shouldBe

type SmallFinite = TypeIntegral (Finite 10)