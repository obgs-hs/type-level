{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

-- |
-- Module      : $Header$
-- Description : First-class families for working with 'Nat's.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for working with 'Nat's.
module Obgs.Fcf.Nat
  ( -- * Type-level variants
    Subtract,
    Div,
    Mod,
    DivMod,

    -- * Comparison
    type (==),

    -- * Operations
    type (^+^),
    type (^-^),
    type (^*^),
    type (^/^),
    type (^%^),
    SubtractMaybe,

    -- * Ranges
    NatsBetween,
    type (...),

    -- * Re-exports
    module Fcf.Data.Nat,
  )
where

import Fcf (Eval, Exp, LiftM2, Pure, TyEq, UnBool, type (=<<), type (@@))
import Fcf.Data.Nat
import Obgs.Fcf.Combinators qualified as Fcf
import Obgs.TL.Nat qualified as TL
import Prelude (Bool, Maybe (..))

-- See the note in 'Obgs.Fcf.Bool' for why the $setup block imports modules it does not use.

-- $setup
-- >>> :m +Fcf.Core
-- >>> :m +Fcf.Combinators
-- >>> :m +Fcf.Class.Bifunctor
-- >>> :m +Fcf.Class.Foldable
-- >>> :m +Fcf.Class.Functor
-- >>> :m +Fcf.Class.Monoid
-- >>> :m +Fcf.Class.Ord
-- >>> :m +Fcf.Data.Bool
-- >>> :m +Fcf.Data.Common
-- >>> :m +Fcf.Data.Function
-- >>> :m +Fcf.Data.List
-- >>> :m +Fcf.Data.Nat
-- >>> :m +Fcf.Data.Symbol
-- >>> :m +Fcf.Utils
-- >>> :m +Fcf
--

-- | A type-level variant of 'Prelude.subtract'.
--
-- >>> :kind! Eval (Subtract 3 5)
-- ...
-- = 2
data Subtract :: Nat -> Nat -> Exp Nat

type instance Eval (Subtract x y) = y TL.- x

-- | A type-level variant of 'Prelude./'.
--
-- >>> :kind! Eval (Div 11 3)
-- ...
-- = 3
data Div :: Nat -> Nat -> Exp Nat

type instance Eval (Div x y) = x `TL.Div` y

-- | A type-level variant of 'Prelude.mod'.
--
-- >>> :kind! Eval (Mod 11 3)
-- ...
-- = 2
data Mod :: Nat -> Nat -> Exp Nat

type instance Eval (Mod x y) = x `TL.Mod` y

-- | A type-level variant of 'Prelude.divMod'.
--
-- >>> :kind! Eval (DivMod 11 3)
-- ...
-- = '(3, 2)
data DivMod :: Nat -> Nat -> Exp (Nat, Nat)

type instance Eval (DivMod x y) = '(x `TL.Div` y, x `TL.Mod` y)

-- | A type-level variant of 'Prelude.==' for 'Nat's.
--
-- >>> :kind! Eval (5 == 5)
-- ...
-- = True
--
-- >>> :kind! Eval (5 == 3)
-- ...
-- = False
data (==) :: Nat -> Nat -> Exp Bool

type instance Eval (x == y) = TyEq x @@ y

infix 4 ==

-- | A lifted variant of 'Prelude.+'.
--
-- >>> :kind! Eval (Pure 5 ^+^ Pure 3)
-- ...
-- = 8
data (^+^) :: Exp Nat -> Exp Nat -> Exp Nat

type instance Eval (x ^+^ y) = Eval (LiftM2 (+) x y)

infixl 6 ^+^

-- | A lifted variant of 'Prelude.-'.
--
-- >>> :kind! Eval (Pure 5 ^-^ Pure 3)
-- ...
-- = 2
data (^-^) :: Exp Nat -> Exp Nat -> Exp Nat

type instance Eval (x ^-^ y) = Eval (LiftM2 (-) x y)

infixl 6 ^-^

-- | A lifted variant of 'Prelude.*'.
--
-- >>> :kind! Eval (Pure 5 ^*^ Pure 3)
-- ...
-- = 15
data (^*^) :: Exp Nat -> Exp Nat -> Exp Nat

type instance Eval (x ^*^ y) = Eval (LiftM2 (*) x y)

infixl 7 ^*^

-- | A lifted variant of 'Div'.
--
-- >>> :kind! Eval (Pure 11 ^/^ Pure 3)
-- ...
-- = 3
data (^/^) :: Exp Nat -> Exp Nat -> Exp Nat

type instance Eval (x ^/^ y) = Eval (LiftM2 Div x y)

-- | A lifted variant of 'Mod'.
--
-- >>> :kind! Eval (Pure 11 ^%^ Pure 3)
-- ...
-- = 2
data (^%^) :: Exp Nat -> Exp Nat -> Exp Nat

type instance Eval (x ^%^ y) = Eval (LiftM2 Mod x y)

-- | Returns 'Just' the result of subtracting two 'Nat's,
-- or 'Nothing' if the result is negative.
--
-- >>> :kind! SubtractMaybe 5 @@ 3
-- ...
-- = Just 2
--
-- >>> :kind! SubtractMaybe 5 @@ 6
-- ...
-- = Nothing
data SubtractMaybe :: Nat -> Nat -> Exp (Maybe Nat)

type instance
  Eval (SubtractMaybe x y) =
    Eval (UnBool (Pure (Just (x TL.- y))) (Pure Nothing) =<< (x < y))

-- | Returns the 'Nat's between two bounds, inclusive.
--
-- >>> :kind! Eval (NatsBetween 3 6)
-- ...
-- = [3, 4, 5, 6]
--
-- >>> :kind! Eval (NatsBetween 6 3)
-- ...
-- = '[]
data NatsBetween :: Nat -> Nat -> Exp [Nat]

type instance
  Eval (NatsBetween from to) =
    UnBool
      (Pure '[])
      (Fcf.IterateN ((+) 1) from (to TL.- from))
      @@ (from TL.<=? to)

-- | An operator variant for 'NatsBetween'.
type (...) (from :: Nat) (to :: Nat) = Eval (NatsBetween from to)