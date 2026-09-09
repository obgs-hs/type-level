{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : $Header$
-- Description : Type utilities for working with tuples.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- Type utilities for working with tuples.
--
-- This module also introduces an orphan 'PEnum' instance for tuples 
-- (albeit with strong requirements).
module Obgs.TL.Tuple
  ( -- * Type-level variants
    Dup,
    Swap,
  )
where

import Data.Finitary (Cardinality, Finitary)
import Data.Singletons.Base.Enum (PBounded (..), PEnum (..))
import Fcf (Eval, type (=<<), type (@@))
import Fcf qualified
import Obgs.Fcf.Enum qualified as Fcf
import Obgs.Fcf.Nat qualified as Fcf
import Obgs.TL.Nat (type (*), type (+))

instance
  ( Finitary a,
    Finitary b,
    PBounded a,
    PBounded b,
    PEnum a,
    PEnum b
  ) =>
  PEnum (a, b)
  where
  type
    Succ '(x, y) =
      Eval
        ( Fcf.UnMaybe (Fcf.Pure '(Succ x, MinBound)) (Fcf.Pure1 ('(,) x))
            =<< Fcf.SuccMaybe y
        )

  type
    Pred '(x, y) =
      Eval
        ( Fcf.UnMaybe (Fcf.Pure '(Pred x, MaxBound)) (Fcf.Pure1 ('(,) x))
            =<< Fcf.PredMaybe y
        )

  type FromEnum @(a, b) '(x, y) = FromEnum x * Cardinality b + FromEnum y

  type
    ToEnum @(a, b) n =
      Eval
        ( Fcf.Bimap Fcf.ToEnum Fcf.ToEnum
            =<< Fcf.DivMod n (Cardinality b)
        )

  type EnumFromTo from to = Fcf.EnumFromToByIx 'Nothing from @@ to

  type EnumFromThenTo from then_ to = Fcf.EnumFromThenToByIx 'Nothing 'Nothing from then_ @@ to

-- | A type-level variant of @dup@.
--
-- >>> :kind! Dup 3
-- ...
-- = '(3, 3)
type Dup (x :: k) = '(x, x)

-- | A type-level variant of 'Data.Tuple.swap'.
--
-- >>> :kind! Swap '(1, 2)
-- ...
-- = '(2, 1)
type family Swap (x :: (k, k')) :: (k', k) where
  Swap '(a, b) = '(b, a)