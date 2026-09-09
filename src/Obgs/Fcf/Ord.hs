{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : First-class families for working with 'Ord' types.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for working with 'Ord' types.
--
-- /All families in this module expect the types involved to have a 'Sing.POrd' instance/.
module Obgs.Fcf.Ord
  ( -- * Type-level variants
    Compare,
    type (<),
    type (<=),
    type (>),
    type (>=),
    type (==),
    Min,
    Max,

    -- * Predicates
    IsBetween,

    -- * Combining comparisons
    type (<>^),
  )
where

import Data.Ord.Singletons qualified as Sing
import Fcf (Else, Eval, Exp, TyEq, type (=<<))
import Obgs.Fcf.Bool (type (&&^))
import Obgs.Fcf.Branch (Case, type (-->))
import Obgs.Fcf.Combinators (ConstM)

-- $setup
-- >>> :m +Fcf.Data.Bool
-- >>> :m +Fcf.Utils
-- >>> import Data.Ord.Singletons qualified as Sing
-- >>> import Prelude.Singletons ()
-- >>> import Fcf hiding (type (<), type (<=), type (>), type (>=))
-- >>> :m +Fcf.Class.Functor

-- | A type-level variant of 'compare'.
--
-- >>> :kind! Eval (Compare 1 2)
-- ...
-- = LT
--
-- >>> :kind! Eval (Compare 2 1)
-- ...
-- = GT
--
-- >>> :kind! Eval (Compare 1 1)
-- ...
-- = EQ
data Compare :: a -> a -> Exp Ordering

type instance Eval (Compare x y) = Sing.Compare x y

-- | A type-level variant of '<'.
--
-- >>> :kind! Eval (1 < 2)
-- ...
-- = True
--
-- >>> :kind! Eval (2 < 1)
-- ...
-- = False
--
-- >>> :kind! Eval (1 < 1)
-- ...
-- = False
data (<) :: a -> a -> Exp Bool

type instance Eval (x < y) = x Sing.< y

infix 4 <

-- | A type-level variant of '<='.
--
-- >>> :kind! Eval (1 <= 2)
-- ...
-- = True
--
-- >>> :kind! Eval (2 <= 1)
-- ...
-- = False
--
-- >>> :kind! Eval (1 <= 1)
-- ...
-- = True
data (<=) :: a -> a -> Exp Bool

type instance Eval (x <= y) = x Sing.<= y

infix 4 <=

-- | A type-level variant of '>'.
--
-- >>> :kind! Eval (1 > 2)
-- ...
-- = False
--
-- >>> :kind! Eval (2 > 1)
-- ...
-- = True
--
-- >>> :kind! Eval (1 > 1)
-- ...
-- = False
data (>) :: a -> a -> Exp Bool

type instance Eval (x > y) = x Sing.> y

infix 4 >

-- | A type-level variant of '>='.
--
-- >>> :kind! Eval (1 >= 2)
-- ...
-- = False
--
-- >>> :kind! Eval (2 >= 1)
-- ...
-- = True
--
-- >>> :kind! Eval (1 >= 1)
-- ...
-- = True
data (>=) :: a -> a -> Exp Bool

type instance Eval (x >= y) = x Sing.>= y

infix 4 >=

-- | A type-level variant of '=='.
data (==) :: a -> a -> Exp Bool

type instance Eval (x == y) = Eval (TyEq x y)

infix 4 ==

-- | A type-level variant of 'min'.
--
-- >>> :kind! Eval (Min 1 2)
-- ...
-- = 1
--
-- >>> :kind! Eval (Min 2 1)
-- ...
-- = 1
data Min :: a -> a -> Exp a

type instance Eval (Min x y) = Sing.Min x y

-- | A type-level variant of 'max'.
--
-- >>> :kind! Eval (Max 1 2)
-- ...
-- = 2
--
-- >>> :kind! Eval (Max 2 1)
-- ...
-- = 2
data Max :: a -> a -> Exp a

type instance Eval (Max x y) = Sing.Max x y

-- | Returns whether a value is located between two others, inclusive.
--
-- >>> :kind! FMap (IsBetween 1 3) @@ '[ 0, 1, 2, 3, 4 ]
-- ...
-- = [False, True, True, True, False]
data IsBetween :: a -> a -> a -> Exp Bool

type instance Eval (IsBetween min max x) = Eval ((min <= x) &&^ (x <= max))

-- | A lifted variant of 'Fcf.Class.Monoid.<>' for 'Ordering's.
--
-- Only evaluates its second argument when the first yields 'EQ'.
--
-- >>> :kind! Eval (Pure 'LT <>^ Pure 'GT)
-- ...
-- = LT
--
-- >>> :kind! Eval (Pure 'EQ <>^ Pure 'GT)
-- ...
-- = GT
--
-- >>> :kind! Eval (Pure 'EQ <>^ Pure 'EQ)
-- ...
-- = EQ
data (<>^) :: Exp Ordering -> Exp Ordering -> Exp Ordering

type instance
  Eval ((<>^) r r') =
    Eval
      ( Case
          '[ 'LT --> 'LT,
             'GT --> 'GT,
             Else (ConstM r')
           ]
          =<< r
      )

infixr 6 <>^