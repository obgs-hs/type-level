{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : First-class families for working with lists.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for working with lists.
module Obgs.Fcf.List
  ( -- Type-level variants
    type (\\),
    Delete,
    Difference,
    Nub,
    Foldl,
    Foldr1,
    Scanl,
    Sequence,

    -- * Predicates
    HasMultipleValues,

    -- * Searching
    ElemIndex,
    ElemBefore,
    ElemAfter,

    -- * Folds
    AndM,
    AreAll,
    AreAny,

    -- * Sets
    IsSet,
    IsSubsetOf,
    SubsetOf,
    ValueInSet,
    ValuesInSet,
    UniqueList,
    CartesianProduct,
    CartesianProduct3,

    -- * Re-exports
    module Fcf.Data.List,
  )
where

import Data.Constraint (Constraint)
import Fcf (Eval, Exp, Flip, LiftM2, Pure, Pure1, TyEq, UnBool, type (<=<), type (=<<), type (@@))
import Fcf.Class.Foldable qualified as Fcf
import Fcf.Class.Functor (FMap)
import Fcf.Data.List
import Obgs.Fcf.Bool (Not, type (&&^))
import {-# SOURCE #-} Obgs.Fcf.Constraint qualified as Fcf
import Obgs.Fcf.Nat (Nat, type (+))
import Obgs.Fcf.Tuple (ConsPair)
import Obgs.TL.Constraint (NoneC)
import Obgs.TL.Error (ErrorMessage ((:$$:)), TypeError, type (:$>:))
import Obgs.TL.Error qualified as TE
import Obgs.TL.List qualified as TL
import Prelude (Bool (..), Maybe (..))

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

-- | A type-level variant of the list difference operator @(\\\\)@.
--
-- >>> :kind! Eval ('[1, 2, 3, 4, 2, 4] \\ '[2, 4])
-- ...
-- = [1, 3, 2, 4]
--
-- >>> :kind! Eval ('[1, 2, 3] \\ '[])
-- ...
-- = [1, 2, 3]
data (\\) :: [a] -> [a] -> Exp [a]

type instance Eval (as \\ bs) = Foldl (Flip Delete) as @@ bs

infix 5 \\

-- | A type-level variant of 'Data.List.delete'.
--
-- >>> :kind! Delete 2 @@ '[1, 2, 3, 2]
-- ...
-- = [1, 3, 2]
data Delete :: a -> [a] -> Exp [a]

type instance Eval (Delete a '[]) = '[]

type instance
  Eval (Delete a (b ': as)) =
    Eval (UnBool (Cons b =<< Delete a as) (Pure as) =<< TyEq a b)

-- | A synonym for @(\\\\)@.
type Difference = (\\)

-- | A type-level variant of 'Data.List.foldl'.
--
-- >>> :kind! Foldl (Flip (Pure2 (:))) '[] @@ '[1, 2, 3]
-- ...
-- = [3, 2, 1]
data Foldl :: (b -> a -> Exp b) -> b -> [a] -> Exp b

type instance Eval (Foldl _ z '[]) = z

type instance
  Eval (Foldl f z (a ': as)) =
    Eval (Foldl f (Eval (f z a)) as)

-- | A type-level variant of 'Data.List.foldr1'.
--
-- __The input list must contain at least one element.__

---- >>> :kind! Foldr1 (+) @@ '[1, 2, 3]
-- ...
-- = 6
--
-- >>> :kind! Foldr1 ((++)) @@ '[ '[1], '[2], '[3] ]
-- ...
-- = [1, 2, 3]
data Foldr1 :: (a -> a -> Exp a) -> [a] -> Exp a

type instance Eval (Foldr1 _ '[x]) = x

type instance Eval (Foldr1 f (x ': y ': xs)) = Eval (f x =<< Foldr1 f (y ': xs))

-- | A type-level variant of 'Data.List.nub'.
--
-- >>> :kind! Nub @@ '[1, 2, 3, 2, 4, 1]
-- ...
-- = [1, 2, 3, 4]
data Nub :: [a] -> Exp [a]

type instance Eval (Nub '[]) = '[]

type instance
  Eval (Nub (a ': as)) =
    Eval (Cons a =<< Nub =<< Filter (Not <=< TyEq a) as)

-- | A type-level variant of 'Data.List.scanl'.
--
-- >>> :kind! Scanl (+) 0 @@ '[1, 2, 3]
-- ...
-- = [0, 1, 3, 6]
--
-- >>> :kind! Scanl (+) 0 @@ '[]
-- ...
-- = '[0]
data Scanl :: (b -> a -> Exp b) -> b -> [a] -> Exp [b]

type instance Eval (Scanl _ z '[]) = '[z]

type instance
  Eval (Scanl f z (a ': as)) =
    Eval
      ( Cons z
          =<< Flip (Scanl f) as
          =<< f z a
      )

-- | A type-level variant of 'Data.List.sequence' for 'Exp's.
--
-- >>> :kind! Sequence @@ '[Pure 1, Pure 2, Pure 3]
-- ...
-- = [1, 2, 3]
data Sequence :: [Exp a] -> Exp [a]

type instance Eval (Sequence '[]) = '[]

type instance Eval (Sequence (a ': as)) = Eval (LiftM2 Cons a (Sequence as))

-- | Returns whether a list contains multiple values.
--
-- >>> :kind! HasMultipleValues @@ '[1, 2]
-- ...
-- = True
--
-- >>> :kind! HasMultipleValues @@ '[1]
-- ...
-- = False
--
-- >>> :kind! HasMultipleValues @@ '[]
-- ...
-- = False
data HasMultipleValues :: [a] -> Exp Bool

type instance Eval (HasMultipleValues as) = TL.HasMultipleValues as

-- | A type-level variant of 'Data.List.elemIndex'.
--
-- >>> :kind! ElemIndex 3 @@ '[1, 2, 3, 4]
-- ...
-- = Just 2
--
-- >>> :kind! ElemIndex 5 @@ '[1, 2, 3, 4]
-- ...
-- = Nothing
data ElemIndex :: a -> [a] -> Exp (Maybe Nat)

type instance Eval (ElemIndex x '[]) = 'Nothing

type instance
  Eval (ElemIndex x (y ': ys)) =
    Eval
      ( UnBool
          (FMap ((+) 1) =<< ElemIndex x ys)
          (Pure ('Just 0))
          =<< TyEq x y
      )

-- | Returns 'Just' the element before a given element in a list, if any.
--
-- Otherwise, returns 'Nothing'.
--
-- >>> :kind! ElemBefore 2 @@ '[1, 2, 3]
-- ...
-- = Just 1
--
-- >>> :kind! ElemBefore 1 @@ '[1, 2, 3]
-- ...
-- = Nothing
--
-- >>> :kind! ElemBefore 4 @@ '[1, 2, 3]
-- ...
-- = Nothing
data ElemBefore :: a -> [a] -> Exp (Maybe a)

type instance Eval (ElemBefore x '[]) = 'Nothing

type instance Eval (ElemBefore x '[y]) = 'Nothing

type instance
  Eval (ElemBefore x (y ': z ': xs)) =
    Eval
      ( UnBool
          (ElemBefore x (z ': xs))
          (Fcf.Pure ('Just y))
          =<< TyEq x z
      )

-- | Returns 'Just' the element after a given element in a list, if any.
--
-- Otherwise, returns 'Nothing'.
--
-- >>> :kind! ElemAfter 2 @@ '[1, 2, 3]
-- ...
-- = Just 3
--
-- >>> :kind! ElemAfter 3 @@ '[1, 2, 3]
-- ...
-- = Nothing
--
-- >>> :kind! ElemAfter 4 @@ '[1, 2, 3]
-- ...
-- = Nothing
data ElemAfter :: a -> [a] -> Exp (Maybe a)

type instance Eval (ElemAfter x '[]) = 'Nothing

type instance Eval (ElemAfter x '[y]) = 'Nothing

type instance
  Eval (ElemAfter x (y ': z ': xs)) =
    Eval
      ( UnBool
          (ElemAfter x (z ': xs))
          (Fcf.Pure ('Just z))
          =<< TyEq x y
      )

-- | A type-level lifted, short-circuiting variant of 'Data.List.and'.
--
-- >>> type T = Pure 'True
-- >>> :kind! AndM @@ '[T, T, T]
-- ...
-- = True
--
-- >>> type F = Pure 'False
-- >>> :kind! AndM @@ '[T, T, T, F]
-- ...
-- = False
data AndM :: [Exp Bool] -> Exp Bool

type instance Eval (AndM '[]) = 'True

type instance Eval (AndM (a ': as)) = Eval (a &&^ AndM as)

-- | A flipped variant of 'Fcf.All'.
--
-- >>> :kind! AreAll '[1, 2, 3] @@ ((Fcf.<) 0)
-- ...
-- = True
--
-- >>> :kind! AreAll '[1, 2, 3] @@ ((Fcf.<) 1)
-- ...
-- = False
data AreAll :: [a] -> (a -> Exp Bool) -> Exp Bool

type instance Eval (AreAll as f) = Eval (Fcf.All f as)

-- | A flipped variant of 'Fcf.Any'.
--
-- >>> :kind! AreAny '[1, 2, 3] @@ ((Fcf.<) 1)
-- ...
-- = True
--
-- >>> :kind! AreAny '[1, 2, 3] @@ ((Fcf.<) 3)
-- ...
-- = False
data AreAny :: [a] -> (a -> Exp Bool) -> Exp Bool

type instance Eval (AreAny as f) = Eval (Fcf.Any f as)

-- | Returns whether each element of a list is unique.
--
-- >>> :kind! IsSet @@ '[1, 2, 3]
-- ...
-- = True
--
-- >>> :kind! IsSet @@ '[1, 2, 3, 2]
-- ...
-- = False
data IsSet :: [a] -> Exp Bool

type instance Eval (IsSet '[]) = 'True

type instance
  Eval (IsSet (a ': as)) =
    Eval ((Not =<< (a `Elem` as)) &&^ IsSet as)

-- | Returns whether the first list is a subset of the second.
-- That is, whether each element of the first list is also an element of the second.
--
-- __Note that duplicates are not taken into account__.
--
-- >>> :kind! Eval ('[1, 2] `IsSubsetOf` '[1, 2, 3])
-- ...
-- = True
--
-- >>> :kind! Eval ('[2, 4] `IsSubsetOf` '[1, 2, 3])
-- ...
-- = False
data IsSubsetOf :: [a] -> [a] -> Exp Bool

type instance Eval (IsSubsetOf '[] '[]) = 'True

type instance Eval (IsSubsetOf '[] (b ': bs)) = 'True

type instance Eval (IsSubsetOf (a ': as) '[]) = 'False

type instance
  Eval (IsSubsetOf (a ': as) (b ': bs)) =
    Eval ((a `Elem` (b ': bs)) &&^ IsSubsetOf as (b ': bs))

-- | Requires a list to be a subset of another list.
--
-- Throws a 'TE.TypeError' if the first list is not a subset of the second.
data SubsetOf :: [a] -> [a] -> Exp Constraint

type instance
  Eval (SubsetOf sub sup) =
    Fcf.UnlessC
      (Eval (sub `IsSubsetOf` sup))
      @@ ThrowSubsetError sub sup

-- | Throws a 'TE.TypeError' warning that a list is not a subset of another.
data ThrowSubsetError :: [a] -> [a] -> Exp Constraint

type instance
  Eval (ThrowSubsetError sub sup) =
    TypeError
      ( TE.Text "List:"
          :$>: TE.PprListH sub
          :$$: TE.Text "is not a subset of list:"
          :$>: TE.PprListH sup
      )

-- | Requires a value to be part of a set.
--
-- Throws a 'TE.TypeError' if the value is not part of the set.
data ValueInSet :: [a] -> a -> Exp Constraint

type instance
  Eval (ValueInSet as a) =
    Fcf.UnlessC
      (Elem a @@ as)
      @@ ThrowValueOutsideSetError as a

-- | Throws a 'TE.TypeError' warning that a value is not part of a set.
data ThrowValueOutsideSetError :: [a] -> a -> Exp Constraint

type instance
  Eval (ThrowValueOutsideSetError as a) =
    TypeError
      ( TE.Text "Value:"
          :$>: TE.ShowType a
          :$$: TE.Text "is not contained in list:"
          :$>: TE.PprListV as
      )

-- | Requires some values to be part of a set.
--
-- Throws a 'TE.TypeError' if any of the values is not part of the set.
data ValuesInSet :: [a] -> [a] -> Exp Constraint

type instance
  Eval (ValuesInSet as bs) =
    Eval (ThrowValuesOutsideSetError as =<< Difference bs as)

-- | Throws a 'TE.TypeError' warning that some values are not part of a set.
--
-- Does nothing if the list is empty.
data ThrowValuesOutsideSetError :: [a] -> [a] -> Exp Constraint

type instance
  Eval (ThrowValuesOutsideSetError _ '[]) =
    NoneC

type instance
  Eval (ThrowValuesOutsideSetError as '[b]) =
    ThrowValueOutsideSetError as @@ b

type instance
  Eval (ThrowValuesOutsideSetError as (b ': b' ': bs)) =
    TypeError
      ( TE.Text "Values:"
          :$>: TE.PprList 3 (b ': b' ': bs)
          :$$: TE.Text "are not contained in list:"
          :$>: TE.PprListV as
      )

-- | Requires a list to contain no duplicates.
--
-- Throws a 'TE.TypeError' if the list contains duplicates.
type UniqueList (as :: [a]) =
  Fcf.UnlessC (IsSet @@ as) @@ ThrowListDuplicatesError as

-- | Throws a 'TE.TypeError' warning that a list contains duplicates.
data ThrowListDuplicatesError :: [a] -> Exp Constraint

type instance
  Eval (ThrowListDuplicatesError as) =
    TypeError
      ( TE.Text "Duplicate elements:"
          :$>: TE.PprListH (Eval (Difference as =<< Nub as))
          :$$: TE.Text "found in list:"
          :$>: TE.PprListH as
      )

-- | Returns the cartesian product of two lists.
--
-- >>> :kind! Eval (CartesianProduct '[1, 2] '[3, 4])
-- ...
-- = ['(1, 3), '(1, 4), '(2, 3), '(2, 4)]
data CartesianProduct :: [a] -> [b] -> Exp [(a, b)]

type instance Eval (CartesianProduct '[] _) = '[]

type instance Eval (CartesianProduct _ '[]) = '[]

type instance
  Eval (CartesianProduct (a ': as) (b ': bs)) =
    Eval
      ( LiftM2
          (++)
          (FMap (Pure1 ('(,) a)) (b ': bs))
          (CartesianProduct as (b ': bs))
      )

-- | Returns the cartesian product of three lists.
--
-- >>> :kind! Eval (CartesianProduct3 '[1, 2] '[3, 4] '[5, 6])
-- ...
-- = ['(1, 3, 5), '(1, 3, 6), '(1, 4, 5), '(1, 4, 6), '(2, 3, 5),
--    '(2, 3, 6), '(2, 4, 5), '(2, 4, 6)]
data CartesianProduct3 :: [a] -> [b] -> [c] -> Exp [(a, b, c)]

type instance Eval (CartesianProduct3 '[] _ _) = '[]

type instance Eval (CartesianProduct3 _ '[] _) = '[]

type instance Eval (CartesianProduct3 _ _ '[]) = '[]

type instance
  Eval (CartesianProduct3 (a ': as) (b ': bs) (c ': cs)) =
    Eval
      ( LiftM2
          (++)
          (FMap (ConsPair a) =<< CartesianProduct (b ': bs) (c ': cs))
          (CartesianProduct3 as (b ': bs) (c ': cs))
      )
