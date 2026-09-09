{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : First-class families for working with 'Constraint's.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for working with 'Constraint's.
module Obgs.Fcf.Constraint
  ( -- * Empty constraints
    NoneC,
    None1C,

    -- * Conditions
    WhenC,
    WhenCM,
    UnlessC,
    UnlessCM,

    -- * Combinations
    BothC,
    Both3C,
    All1C,
  )
where

import Data.Constraint (Constraint)
import Fcf (Eval, Exp, Flip, Pure1, type (=<<))
import Fcf.Class.Functor (FMap)
import Obgs.Fcf.List (Sequence)
import Prelude (Bool (..))

-- See the note in 'Obgs.Fcf.Bool' for why the $setup block imports modules it does not use.

-- $setup
-- >>> :m +Fcf
-- >>> :m +Fcf.Data.Bool
-- >>> :m +Fcf.Utils
-- >>> :m +Obgs.Fcf.List
-- >>> :m +Data.Kind

-- | A 'Constraint' that always holds.
--
-- >>> :kind! Eval NoneC
-- ...
-- = () :: Constraint
data NoneC :: Exp Constraint

type instance Eval NoneC = ()

-- | A 'Constraint' that holds for any type.
--
-- >>> :kind! Eval (None1C Int)
-- ...
-- = () :: Constraint
data None1C :: k -> Exp Constraint

type instance Eval (None1C _) = ()

-- | Requires a specific 'Constraint' when a predicate is 'True'.
--
-- >>> :kind! Eval (WhenC 'True (Pure (Show Int)))
-- ...
-- = Show Int
--
-- >>> :kind! Eval (WhenC 'False (Pure (Show Int)))
-- ...
-- = () :: Constraint
data WhenC :: Bool -> Exp Constraint -> Exp Constraint

type instance Eval (WhenC 'True c) = Eval c

type instance Eval (WhenC 'False _) = ()

-- | A variant of 'WhenC' with a lifted predicate.
--
-- >>> :kind! Eval (WhenCM (Pure 'True) (Pure (Show Int)))
-- ...
-- = Show Int
data WhenCM :: Exp Bool -> Exp Constraint -> Exp Constraint

type instance Eval (WhenCM p c) = Eval (Flip WhenC c =<< p)

-- | Requires a specific 'Constraint' unless a predicate is 'True'.
--
-- >>> :kind! Eval (UnlessC 'True (Pure (Show Int)))
-- ...
-- = () :: Constraint
data UnlessC :: Bool -> Exp Constraint -> Exp Constraint

type instance Eval (UnlessC 'True _) = ()

type instance Eval (UnlessC 'False c) = Eval c

-- | A variant of 'UnlessC' with a lifted predicate.
data UnlessCM :: Exp Bool -> Exp Constraint -> Exp Constraint

type instance Eval (UnlessCM p c) = Eval (Flip UnlessC c =<< p)

-- | Requires two 'Constraint's.
--
-- >>> :kind! Eval (BothC (Show Int) (Eq Int))
-- ...
-- = (Show Int, Eq Int)
data BothC :: Constraint -> Constraint -> Exp Constraint

type instance Eval (BothC a b) = (a, b)

-- | Requires three 'Constraint's.
--
-- >>> :kind! Eval (Both3C (Show Int) (Eq Int) (Ord Int))
-- ...
-- = (Show Int, Eq Int, Ord Int)
data Both3C :: Constraint -> Constraint -> Constraint -> Exp Constraint

type instance Eval (Both3C a b c) = (a, b, c)

-- | Requires a set of 'Constraint's.
data AllC :: [Constraint] -> Exp Constraint

type instance Eval (AllC '[]) = ()

type instance Eval (AllC (c ': cs)) = (c, Eval (AllC cs))

-- | Requires a 'Constraint' to hold for all elements in a list.
data All1C :: (a -> Exp Constraint) -> [a] -> Exp Constraint

type instance Eval (All1C c as) = Eval (AllC =<< Sequence =<< FMap (Pure1 c) as)