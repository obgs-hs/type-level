{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : $Header$
-- Description : Type utilities for working with 'Nat's.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- Type utilities for working with 'Nat's.
module Obgs.TL.Nat
  ( -- * Comparison
    type (==),
    type (<=?),
    type (<?),
    type (>=?),
    type (>?),

    -- * Operations
    SubtractMaybe,

    -- * Constraints
    KnownNats,

    -- * Re-exports
    module GHC.TypeNats,
  )
where

import Data.Type.Bool (If, Not)
import GHC.TypeNats
import Obgs.TL.Constraint (All1C)
import Obgs.TL.Type (type (==))
import Prelude (Maybe (..), Ordering (..))

-- | A type-level variant of 'Prelude.<'.
type (x :: Nat) <? (y :: Nat) = CmpNat x y == 'LT

infix 4 <?

-- | A type-level variant of 'Prelude.>='.
type (x :: Nat) >=? (y :: Nat) = Not (x <? y)

infix 4 >=?

-- | A type-level variant of 'Prelude.>'.
type (x :: Nat) >? (y :: Nat) = CmpNat x y == 'GT

infix 4 >?

-- | Subtracts a 'Nat' from another.
--
-- Returns 'Nothing' if the result would be negative.
--
-- >>> :kind! SubtractMaybe 3 5
-- ...
-- = Just 2
--
-- >>> :kind! SubtractMaybe 5 3
-- ...
-- = Nothing
type SubtractMaybe (x :: Nat) (y :: Nat) = If (x >? y) 'Nothing ('Just (y - x))

-- | Requires 'KnownNat' instances for a list of 'Nat's.
type KnownNats (ns :: [Nat]) = All1C KnownNat ns