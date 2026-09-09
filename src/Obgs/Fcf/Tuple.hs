-- |
-- Module      : $Header$
-- Description : First-class families for working with tuples.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for working with tuples.
module Obgs.Fcf.Tuple
  ( -- * Type-level variants
    First,
    Second,
    First3,
    Second3,
    Third3,
    Swap,

    -- * Mapping
    Trimap,

    -- * (Un)currying
    Curry,
    Curry3,
    Uncurry3,

    -- * Flattening and splitting
    Flatten3L,
    Flatten3R,
    Split3L,
    Split3R,
    ShiftSplit3L,
    ShiftSplit3R,

    -- * Reordering
    Permutations,
    Permutations3,

    -- * Expanding
    Dup,
    DupL,
    DupR,
    ConsPair,

    -- * Shrinking
    Drop3L,
    Drop3M,
    Drop3R,

    -- * Re-exports
    Fst,
    Snd,
    Uncurry,
  )
where

import Fcf (Eval, Exp, Fst, Snd, Uncurry, type (@@))

-- $setup
-- >>> :m +Fcf
-- >>> :m +Fcf.Data.Nat

-- | A type-level variant of 'Data.Bifunctor.first'.
--
-- >>> :kind! First ((+) 2) @@ '(1, 2)
-- ...
-- = '(3, 2)
data First :: (a -> Exp c) -> (a, b) -> Exp (c, b)

type instance Eval (First f '(x, y)) = '(f @@ x, y)

-- | A type-level variant of 'Data.Bifunctor.second'.
--
-- >>> :kind! Second ((+) 2) @@ '(1, 2)
-- ...
-- = '(1, 4)
data Second :: (b -> Exp c) -> (a, b) -> Exp (a, c)

type instance Eval (Second f '(x, y)) = '(x, f @@ y)

-- | Maps the first element of a 3-tuple.
--
-- >>> :kind! First3 ((+) 4) @@ '(1, 2, 3)
-- ...
-- = '(5, 2, 3)
data First3 :: (a -> Exp d) -> (a, b, c) -> Exp (d, b, c)

type instance Eval (First3 f '(x, y, z)) = '(f @@ x, y, z)

-- | Maps the second element of a 3-tuple.
--
-- >>> :kind! Second3 ((+) 4) @@ '(1, 2, 3)
-- ...
-- = '(1, 6, 3)
data Second3 :: (b -> Exp d) -> (a, b, c) -> Exp (a, d, c)

type instance Eval (Second3 f '(x, y, z)) = '(x, f @@ y, z)

-- | Maps the third element of a 3-tuple.
--
-- >>> :kind! Third3 ((+) 4) @@ '(1, 2, 3)
-- ...
-- = '(1, 2, 7)
data Third3 :: (c -> Exp d) -> (a, b, c) -> Exp (a, b, d)

type instance Eval (Third3 f '(x, y, z)) = '(x, y, f @@ z)

-- | A type-level variant of 'Data.Tuple.swap'.
--
-- >>> :kind! Swap @@ '(1, 2)
-- ...
-- = '(2, 1)
data Swap :: (a, b) -> Exp (b, a)

type instance Eval (Swap '(x, y)) = '(y, x)

-- | Applies a set of three functions to the elements of a 3-tuple.
--
-- >>> :kind! Trimap ((+) 1) ((+) 2) ((+) 3) @@ '(1, 2, 3)
-- ...
-- = '(2, 4, 6)
data Trimap :: (a -> Exp d) -> (b -> Exp e) -> (c -> Exp f) -> (a, b, c) -> Exp (d, e, f)

type instance Eval (Trimap f g h '(x, y, z)) = '(f @@ x, g @@ y, h @@ z)

-- | A type-level variant of 'curry'.
--
-- >>> :kind! Eval (Curry Fst 1 2)
-- ...
-- = 1
data Curry :: ((a, b) -> Exp c) -> a -> b -> Exp c

type instance Eval (Curry f x y) = Eval (f '(x, y))

-- | Converts a function on a 3-tuple to a function on three arguments.
--
-- >>> :kind! Eval (Curry3 Split3L 1 2 3)
-- ...
-- = '( '(1, 2), 3)
data Curry3 :: ((a, b, c) -> Exp d) -> a -> b -> c -> Exp d

type instance Eval (Curry3 f x y z) = Eval (f '(x, y, z))

-- | Converts a function on three arguments to a function on a 3-tuple.
--
-- >>> :kind! Eval (Uncurry3 (Curry3 Split3L) '(1, 2, 3))
-- ...
-- = '( '(1, 2), 3)
data Uncurry3 :: (a -> b -> c -> Exp d) -> (a, b, c) -> Exp d

type instance Eval (Uncurry3 f '(x, y, z)) = Eval (f x y z)

-- | Flattens a "left-biased" nested tuple into a 3-tuple.
--
-- >>> :kind! Flatten3L @@ '( '(1, 2), 3 )
-- ...
-- = '(1, 2, 3)
data Flatten3L :: ((a, b), c) -> Exp (a, b, c)

type instance Eval (Flatten3L '( '(x, y), z)) = '(x, y, z)

-- | Flattens a "right-biased" nested tuple into a 3-tuple.
--
-- >>> :kind! Flatten3R @@ '( 1, '(2, 3) )
-- ...
-- = '(1, 2, 3)
data Flatten3R :: (a, (b, c)) -> Exp (a, b, c)

type instance Eval (Flatten3R '(x, '(y, z))) = '(x, y, z)

-- | Splits a 3-tuple into a "left-biased" nested tuple.
--
-- >>> :kind! Split3L @@ '(1, 2, 3)
-- ...
-- = '( '(1, 2), 3)
data Split3L :: (a, b, c) -> Exp ((a, b), c)

type instance Eval (Split3L '(x, y, z)) = '( '(x, y), z)

-- | Splits a 3-tuple into a "right-biased" nested tuple.
--
-- >>> :kind! Split3R @@ '(1, 2, 3)
-- ...
-- = '(1, '(2, 3))
data Split3R :: (a, b, c) -> Exp (a, (b, c))

type instance Eval (Split3R '(x, y, z)) = '(x, '(y, z))

-- | Converts a nested tuple from "right-biased" to "left-biased".
--
-- >>> :kind! ShiftSplit3L @@ '(1, '(2, 3))
-- ...
-- = '( '(1, 2), 3)
data ShiftSplit3L :: (a, (b, c)) -> Exp ((a, b), c)

type instance Eval (ShiftSplit3L '(x, '(y, z))) = '( '(x, y), z)

-- | Converts a nested tuple from "left-biased" to "right-biased".
--
-- >>> :kind! ShiftSplit3R @@ '( '(1, 2), 3)
-- ...
-- = '(1, '(2, 3))
data ShiftSplit3R :: ((a, b), c) -> Exp (a, (b, c))

type instance Eval (ShiftSplit3R '( '(x, y), z)) = '(x, '(y, z))

-- | Returns all possible permutations of a pair.
--
-- >>> :kind! Permutations @@ '(1, 2)
-- ...
-- = ['(1, 2), '(2, 1)]
data Permutations :: (a, a) -> Exp [(a, a)]

type instance Eval (Permutations '(x, y)) = '[ '(x, y), '(y, x)]

-- | Returns all possible permutations of a 3-tuple.
--
-- >>> :kind! Permutations3 @@ '(1, 2, 3)
-- ...
-- = ['(1, 2, 3), '(1, 3, 2), '(2, 1, 3), '(2, 3, 1), '(3, 1, 2),
--    '(3, 2, 1)]
data Permutations3 :: (a, a, a) -> Exp [(a, a, a)]

type instance
  Eval (Permutations3 '(x, y, z)) =
    '[ '(x, y, z),
       '(x, z, y),
       '(y, x, z),
       '(y, z, x),
       '(z, x, y),
       '(z, y, x)
     ]

-- | Duplicates a value into a pair.
--
-- >>> :kind! Dup @@ 1
-- ...
-- = '(1, 1)
data Dup :: a -> Exp (a, a)

type instance Eval (Dup a) = '(a, a)

-- | Duplicates the leftmost element of a pair.
--
-- >>> :kind! DupL @@ '(1, 2)
-- ...
-- = '(1, 1, 2)
data DupL :: (a, b) -> Exp (a, a, b)

type instance Eval (DupL '(x, y)) = '(x, x, y)

-- | Duplicates the rightmost element of a pair.
--
-- >>> :kind! DupR @@ '(1, 2)
-- ...
-- = '(1, 2, 2)
data DupR :: (a, b) -> Exp (a, b, b)

type instance Eval (DupR '(x, y)) = '(x, y, y)

-- | Adds an element to the front of a pair, resulting in a 3-tuple.
--
-- >>> :kind! ConsPair 1 @@ '(2, 3)
-- ...
-- = '(1, 2, 3)
data ConsPair :: k -> (k', k'') -> Exp (k, k', k'')

type instance Eval (ConsPair a '(b, c)) = '(a, b, c)

-- | Drops the leftmost element of a 3-tuple.
--
-- >>> :kind! Drop3L @@ '(1, 2, 3)
-- ...
-- = '(2, 3)
data Drop3L :: (a, b, c) -> Exp (b, c)

type instance Eval (Drop3L '(_, x, y)) = '(x, y)

-- | Drops the middle element of a 3-tuple.
--
-- >>> :kind! Drop3M @@ '(1, 2, 3)
-- ...
-- = '(1, 3)
data Drop3M :: (a, b, c) -> Exp (a, c)

type instance Eval (Drop3M '(x, _, y)) = '(x, y)

-- | Drops the rightmost element of a 3-tuple.
--
-- >>> :kind! Drop3R @@ '(1, 2, 3)
-- ...
-- = '(1, 2)
data Drop3R :: (a, b, c) -> Exp (a, b)

type instance Eval (Drop3R '(x, y, _)) = '(x, y)