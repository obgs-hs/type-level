{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : Type utilities for working with 'Constraint's.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- Type utilities for working with 'Constraint's.
module Obgs.TL.Constraint
  ( -- * Empty constraints
    NoneC,
    None1C,

    -- * Conditions
    WhenC,
    UnlessC,

    -- * Combinations
    AllC,
    All1C,

    -- * Re-exports
    Constraint,
  )
where

import Data.Constraint (Constraint)
import Data.Type.Bool (If)
import Prelude

-- $setup
-- >>> :m +Data.Kind

-- | A 'Constraint' that always holds.
type NoneC = () :: Constraint

-- | A 'Constraint' that holds for any type.
class None1C (a :: k)

instance None1C a

-- | Requires a set of 'Constraint's.
--
-- >>> :kind! AllC '[]
-- ...
-- = () :: Constraint
--
-- >>> :kind! AllC '[Show Int, Eq Int]
-- ...
-- = (Show Int, (Eq Int, () :: Constraint))
type family AllC (cs :: [Constraint]) :: Constraint where
  AllC '[] = NoneC
  AllC (c ': cs) = (c, AllC cs)

-- | Requires a 'Constraint' to hold for all types in a list.
--
-- >>> :kind! All1C Show '[Int, Bool]
-- ...
-- = (Show Int, (Show Bool, () :: Constraint))
type family All1C (f :: k -> Constraint) (as :: [k]) :: Constraint where
  All1C _ '[] = NoneC
  All1C f (a ': as) = (f a, All1C f as)

-- | Requires a specific 'Constraint' when a predicate is 'True'.
--
-- >>> :kind! WhenC 'True (Show Int)
-- ...
-- = Show Int
type WhenC (p :: Bool) (c :: Constraint) = If p c NoneC

-- | Requires a specific 'Constraint' unless a predicate is 'True'.
--
-- >>> :kind! UnlessC 'True (Show Int)
-- ...
-- = () :: Constraint
type UnlessC (p :: Bool) (c :: Constraint) = If p NoneC c
