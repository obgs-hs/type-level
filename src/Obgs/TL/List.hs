{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : Type utilities for working with lists.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- Type utilities for working with lists.
module Obgs.TL.List
  ( -- * Type-level variants
    type (!!),
    type (!?),
    type (++),
    Drop,
    Elem,
    Foldr,
    Foldr1,
    Head,
    Last,
    Intersect,
    Length,
    Map,
    Nub,
    Repeat,
    Replicate,
    Reverse,
    Take,

    -- * Mapping
    Indexed,

    -- * Set operations
    DeleteAll,

    -- * Predicates
    HasMultipleValues,
  )
where

import Data.Type.Bool (If)
import Obgs.TL.Nat (Nat, type (+), type (-))
import Prelude

-- $setup
-- >>> :m +Obgs.TL.Type

-- | A type-level variant of 'Data.List.!!'.
--
-- >>> :kind! '[1, 2, 3] !! 1
-- ...
-- = 2
type family (as :: [k]) !! (n :: Nat) :: k where
  (a ': _) !! 0 = a
  (_ ': as) !! n = as !! (n - 1)

infixl 9 !!

-- | A type-level variant of 'Data.List.!?'.
--
-- >>> :kind! '[1, 2, 3] !? 1
-- ...
-- = Just 2
--
-- >>> :kind! '[1, 2, 3] !? 4
-- ...
-- = Nothing
type family (as :: [k]) !? (n :: Nat) :: Maybe k where
  '[] !? _ = 'Nothing
  (a ': _) !? 0 = 'Just a
  (_ ': as) !? n = as !? (n - 1)

infixl 9 !?

-- | A type-level variant of 'Data.List.++'.
--
-- >>> :kind! '[1, 2] ++ '[3, 4]
-- ...
-- = [1, 2, 3, 4]
type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  as ++ '[] = as
  (a ': as) ++ bs = a ': (as ++ bs)

infixr 5 ++

-- | Deletes all occurrences of an element from a list.
--
-- >>> :kind! DeleteAll 3 '[1, 2, 3, 4, 3]
-- ...
-- = [1, 2, 4]
type family DeleteAll (a :: k) (as :: [k]) :: [k] where
  DeleteAll _ '[] = '[]
  DeleteAll a (a ': as) = DeleteAll a as
  DeleteAll a (b ': as) = b ': DeleteAll a as

-- | A type-level variant of 'Data.List.drop'.
--
-- >>> :kind! Drop 2 '[1, 2, 3, 4]
-- ...
-- = [3, 4]
type family Drop (n :: Nat) (as :: [k]) :: [k] where
  Drop 0 as = as
  Drop n '[] = '[]
  Drop n (_ ': as) = Drop (n - 1) as

-- | A type-level variant of 'Data.List.elem'.
--
-- >>> :kind! Elem 2 '[1, 2, 3]
-- ...
-- = True
--
-- >>> :kind! Elem 4 '[1, 2, 3]
-- ...
-- = False
type family Elem (a :: k) (as :: [k]) :: Bool where
  Elem _ '[] = 'False
  Elem a (a ': as) = 'True
  Elem a (_ ': as) = Elem a as

-- | A type-level variant of 'Data.List.foldr'.
--
-- >>> :kind! Foldr (:) '[] '[1, 2, 3]
-- ...
-- = [1, 2, 3]
type family Foldr (f :: k -> k' -> k') (z :: k') (as :: [k]) :: k' where
  Foldr _ z '[] = z
  Foldr f z (a ': as) = f a (Foldr f z as)

-- | A type-level variant of 'Data.List.foldr1'.
--
-- >>> data Test a = Base a | Test a :<>: Test a
-- >>> :kind! Foldr1 (:<>:) '[Base 1, Base 2, Base 3]
-- ...
-- = Base 1 :<>: (Base 2 :<>: Base 3)
type family Foldr1 (f :: k -> k -> k) (as :: [k]) :: k where
  Foldr1 _ (a ': '[]) = a
  Foldr1 f (a ': as) = f a (Foldr1 f as)

-- | Returns whether a list contains two or more values.
--
-- >>> :kind! HasMultipleValues '[1, 2, 3]
-- ...
-- = True
--
-- >>> :kind! HasMultipleValues '[1]
-- ...
-- = False
--
-- >>> :kind! HasMultipleValues '[]
-- ...
-- = False
type family HasMultipleValues (as :: [k]) :: Bool where
  HasMultipleValues (a ': b ': _) = True
  HasMultipleValues _ = False

-- | A type-level variant of 'Data.List.head'.
--
-- >>> :kind! Head '[1, 2, 3]
-- ...
-- = 1
type family Head (as :: [k]) :: k where
  Head (a ': _) = a

-- | A type-level variant of 'Data.List.intersect'.
--
-- >>> :kind! Intersect '[1, 2, 3, 4, 3] '[6, 4, 3, 5, 4]
-- ...
-- = [3, 4, 3]
type family Intersect (as :: [k]) (bs :: [k]) :: [k] where
  Intersect '[] _ = '[]
  Intersect _ '[] = '[]
  Intersect (a ': as) bs = If (a `Elem` bs) (a ': Intersect as bs) (Intersect as bs)

-- | Indexes a type-level list, starting from 0.
--
-- >>> :kind! Indexed '[ "a", "b", "c" ]
-- ...
-- = ['(0, "a"), '(1, "b"), '(2, "c")]
type family Indexed (as :: [k]) :: [(Nat, k)] where
  Indexed as = Indexed' as 0

type family Indexed' (as :: [k]) (start :: Nat) :: [(Nat, k)] where
  Indexed' '[] _ = '[]
  Indexed' (a ': as) n = '(n, a) ': Indexed' as (n + 1)

-- | A type-level variant of 'Data.List.last'.
--
-- >>> :kind! Last '[1, 2, 3]
-- ...
-- = 3
type family Last (as :: [k]) :: k where
  Last '[a] = a
  Last (_ ': as) = Last as

-- | A type-level variant of 'Data.List.length'.
--
-- >>> :kind! Length '[1, 2, 3]
-- ...
-- = 3
--
-- >>> :kind! Length '[]
-- ...
-- = 0
type family Length (as :: [k]) :: Nat where
  Length '[] = 0
  Length (a ': as) = 1 + Length as

-- | A type-level variant of 'Data.List.map'.
--
-- >>> :kind! Map 'Just '[1, 2, 3]
-- ...
-- = [Just 1, Just 2, Just 3]
type family Map (f :: k -> k') (as :: [k]) :: [k'] where
  Map _ '[] = '[]
  Map f (a ': as) = f a ': Map f as

-- | A type-level variant of 'Data.List.nub'.
--
-- >>> :kind! Nub '[1, 2, 1, 3, 2, 1, 4]
-- ...
-- = [1, 2, 3, 4]
type family Nub (as :: [k]) :: [k] where
  Nub '[] = '[]
  Nub (a ': as) = a ': Nub (DeleteAll a as)

-- | A type-level variant of 'Data.List.repeat'.
--
-- Note that this has very few interesting use cases, as it will infinitely recurse.
type family Repeat (a :: k) :: [k] where
  Repeat a = a ': Repeat a

-- | A type-level variant of 'Data.List.replicate'.
--
-- >>> :kind! Replicate 3 1
-- ...
-- = [1, 1, 1]
type family Replicate (n :: Nat) (a :: k) :: [k] where
  Replicate 0 _ = '[]
  Replicate n a = a ': Replicate (n - 1) a

-- | A type-level variant of 'Data.List.reverse'.
--
-- >>> :kind! Reverse '[1, 2, 3]
-- ...
-- = [3, 2, 1]
type family Reverse (as :: [k]) :: [k] where
  Reverse '[] = '[]
  Reverse (a ': as) = Reverse as ++ '[a]

-- | A type-level variant of 'Data.List.take'.
--
-- >>> :kind! Take 2 '[1, 2, 3, 4]
-- ...
-- = [1, 2]
type family Take (n :: Nat) (as :: [k]) :: [k] where
  Take 0 _ = '[]
  Take n '[] = '[]
  Take n (a ': as) = a ': Take (n - 1) as