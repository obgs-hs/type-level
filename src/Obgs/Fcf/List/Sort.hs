{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : First-class families for sorting lists.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for sorting lists, using a stable merge sort.
module Obgs.Fcf.List.Sort
  ( -- * Sorting
    Sort,
    SortBy,
  )
where

import Fcf (Eval, Exp)
import Obgs.Fcf.Ord (Compare)
import Prelude (Ordering (..))

-- See the note in 'Obgs.Fcf.Bool' for why the $setup block imports modules it does not use.

-- $setup
-- >>> :m +Fcf
-- >>> :m +Fcf.Data.Bool
-- >>> :m +Fcf.Utils
-- >>> :m +Obgs.Fcf.Ord
-- >>> import Prelude.Singletons ()

-- | A type-level variant of 'Data.List.sort'.
--
-- Orders elements using 'Obgs.Fcf.Ord.Compare', so the element type needs a
-- 'Data.Ord.Singletons.POrd' instance.
--
-- >>> :kind! Eval (Sort '[5, 3, 1, 9, 4, 6, 3])
-- ...
-- = [1, 3, 3, 4, 5, 6, 9]
--
-- >>> :kind! Eval (Sort '["b", "a", "c"])
-- ...
-- = ["a", "b", "c"]
data Sort :: [a] -> Exp [a]

type instance Eval (Sort xs) = Eval (SortBy Compare xs)

-- | A type-level variant of 'Data.List.sortBy'.
--
-- The sort is stable: elements that compare to 'EQ' keep their relative order.
--
-- >>> :kind! Eval (SortBy Compare '[5, 3, 1, 9, 4, 6, 3])
-- ...
-- = [1, 3, 3, 4, 5, 6, 9]
--
-- >>> :kind! Eval (SortBy (Flip Compare) '[5, 3, 1, 9, 4, 6, 3])
-- ...
-- = [9, 6, 5, 4, 3, 3, 1]
data SortBy :: (a -> a -> Exp Ordering) -> [a] -> Exp [a]

type instance Eval (SortBy _ '[]) = '[]

type instance Eval (SortBy _ '[x]) = '[x]

type instance
  Eval (SortBy cmp (x ': y ': zs)) =
    SortHalves cmp (Eval (Split (x ': y ': zs)))

-- | Sorts both halves of a split list, then merges them.
type family SortHalves (cmp :: a -> a -> Exp Ordering) (halves :: ([a], [a])) :: [a] where
  SortHalves cmp '(ls, rs) =
    Eval (MergeBy cmp (Eval (SortBy cmp ls)) (Eval (SortBy cmp rs)))

-- | Merges two lists that are already sorted.
data Merge :: [a] -> [a] -> Exp [a]

type instance Eval (Merge xs ys) = Eval (MergeBy Compare xs ys)

-- | Merges two lists that are already sorted, by the given comparison.
--
-- Both lists are expected to be sorted by that same comparison.
data MergeBy :: (a -> a -> Exp Ordering) -> [a] -> [a] -> Exp [a]

type instance Eval (MergeBy _ '[] ys) = ys

type instance Eval (MergeBy _ (x ': xs) '[]) = x ': xs

type instance
  Eval (MergeBy cmp (x ': xs) (y ': ys)) =
    MergeStep (Eval (cmp x y)) cmp x xs y ys

type family
  MergeStep
    (ordering :: Ordering)
    (cmp :: a -> a -> Exp Ordering)
    (x :: a)
    (xs :: [a])
    (y :: a)
    (ys :: [a]) ::
    [a]
  where
  MergeStep 'GT cmp x xs y ys = 
    y ': Eval (MergeBy cmp (x ': xs) ys)
  MergeStep _ cmp x xs y ys = 
    x ': Eval (MergeBy cmp xs (y ': ys))

-- | Splits a list into two lists of near-equal length, by dealing the elements out alternately.
data Split :: [a] -> Exp ([a], [a])

type instance Eval (Split '[]) = '( '[], '[])

type instance Eval (Split '[x]) = '( '[x], '[])

type instance Eval (Split (x ': y ': zs)) = SplitStep x y (Eval (Split zs))

type family SplitStep (x :: a) (y :: a) (halves :: ([a], [a])) :: ([a], [a]) where
  SplitStep x y '(ls, rs) = '(x ': ls, y ': rs)
