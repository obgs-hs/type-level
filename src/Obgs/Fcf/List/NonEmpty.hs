{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : $Header$
-- Description : First-class families for working with 'NonEmpty' lists.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for working with 'NonEmpty' lists.
module Obgs.Fcf.List.NonEmpty
  ( -- * Type-level variants
    Foldr1,
    Head,
    Intersperse,
    Last,
    Singleton,
    TakeWhile,
    ToList,

    -- * Conversion
    FromList,
    UnsafeFromList,

    -- * Predicates
    HasMultipleValues,

    -- * Searching
    ElemBefore,
    ElemAfter,
  )
where

import Data.List.NonEmpty
import Fcf (Eval, Exp, FromMaybe, LiftM2, Map, Pure, Pure2, TyEq, UnBool, type (=<<), type (@@))
import Obgs.Fcf.Foldable qualified as Fold
import Obgs.Fcf.List qualified as L
import Prelude (Bool, Maybe (..))

-- See the note in 'Obgs.Fcf.Bool' for why the $setup block imports modules it does not use.

-- $setup
-- >>> :m +Fcf.Core
-- >>> :m +Fcf.Combinators
-- >>> :m +Fcf.Class.Bifunctor
-- >>> :m +Fcf.Class.Foldable
-- >>> :m +Fcf.Class.Functor
-- >>> :m +Fcf.Class.Monoid
-- >>> import Fcf.Class.Ord hiding (type (>=))
-- >>> :m +Fcf.Data.Bool
-- >>> :m +Fcf.Data.Common
-- >>> :m +Fcf.Data.Function
-- >>> import Fcf.Data.List hiding (Head, Intersperse, Last, Singleton, TakeWhile)
-- >>> :m +Fcf.Data.Nat
-- >>> :m +Fcf.Data.Symbol
-- >>> :m +Fcf.Utils
-- >>> import Fcf hiding (Head, Intersperse, Last, Singleton, TakeWhile)
-- >>> :m +Data.List.NonEmpty
--

-- Orphan instances.

type instance Eval (Map f (x ':| xs)) = Eval (LiftM2 (Pure2 (:|)) (f x) (Map f xs))

type instance Eval (Fold.Foldr f z (x ':| xs)) = Eval (Fold.Foldr f z (x ': xs))

type instance Eval (Fold.FoldMap f (x ':| xs)) = Eval (Fold.FoldMap f (x ': xs))

-- | Converts a list to a 'NonEmpty' list.
--
-- A type-level variant of 'Data.List.NonEmpty.nonEmpty'.
data FromList :: [a] -> Exp (Maybe (NonEmpty a))

type instance Eval (FromList '[]) = 'Nothing

type instance Eval (FromList (x ': xs)) = 'Just (x ':| xs)

-- | Converts a list to a 'NonEmpty' list.
--
-- __Gets stuck if the input list is empty.__
--
-- A type-level variant of 'Data.List.NonEmpty.fromList'.
data UnsafeFromList :: [a] -> Exp (NonEmpty a)

type instance Eval (UnsafeFromList (x ': xs)) = x ':| xs

-- | A type-level variant of 'Data.List.NonEmpty.foldr1'.
--
-- >>> :kind! Foldr1 (+) @@ (1 ':| '[2, 3])
-- ...
-- = 6
--
-- >>> :kind! Foldr1 (++) @@ ('[1] ':| '[ '[2], '[3]])
-- ...
-- = [1, 2, 3]
data Foldr1 :: (a -> a -> Exp a) -> NonEmpty a -> Exp a

type instance Eval (Foldr1 f (x ':| '[])) = x

type instance Eval (Foldr1 f (x ':| (y ': xs))) = Eval (f x =<< Foldr1 f (y ':| xs))

-- | A type-level variant of 'Data.List.NonEmpty.head'.
--
-- >>> :kind! Head @@ (1 ':| '[2, 3])
-- ...
-- = 1
data Head :: NonEmpty a -> Exp a

type instance Eval (Head (x ':| xs)) = x

-- | A type-level variant of 'Data.List.NonEmpty.intersperse'.
--
-- >>> :kind! Intersperse 0 @@ (1 ':| '[2, 3])
-- ...
-- = 1 :| [0, 2, 0, 3]
data Intersperse :: a -> NonEmpty a -> Exp (NonEmpty a)

type instance
  Eval (Intersperse x (y ':| ys)) =
    Eval (UnsafeFromList =<< L.Intersperse x (y ': ys))

-- | A type-level variant of 'Data.List.NonEmpty.last'.
--
-- >>> :kind! Last @@ (1 ':| '[2, 3])
-- ...
-- = 3
--
-- >>> :kind! Last @@ (1 ':| '[])
-- ...
-- = 1
data Last :: NonEmpty a -> Exp a

type instance Eval (Last (x ':| xs)) = Eval (FromMaybe x =<< L.Last (x ': xs))

-- | A type-level variant of 'Data.List.NonEmpty.singleton'.
data Singleton :: a -> Exp (NonEmpty a)

type instance Eval (Singleton x) = x ':| '[]

-- | A type-level variant of 'Data.List.NonEmpty.takeWhile'.
--
-- >>> :kind! TakeWhile ((>=) 2) @@ (1 ':| '[2, 3, 4])
-- ...
-- = [1, 2]
data TakeWhile :: (a -> Exp Bool) -> NonEmpty a -> Exp [a]

type instance Eval (TakeWhile p (x ':| xs)) = Eval (L.TakeWhile p (x ': xs))

-- | A type-level variant of 'Data.List.NonEmpty.toList'.
--
-- >>> :kind! ToList @@ (1 ':| '[2, 3])
-- ...
-- = [1, 2, 3]
data ToList :: NonEmpty a -> Exp [a]

type instance Eval (ToList (x :| xs)) = x ': xs

-- | Returns whether a list contains multiple values.
--
-- >>> :kind! HasMultipleValues @@ (1 ':| '[2])
-- ...
-- = True
--
-- >>> :kind! HasMultipleValues @@ (1 ':| '[])
-- ...
-- = False
data HasMultipleValues :: NonEmpty a -> Exp Bool

type instance Eval (HasMultipleValues (x ':| xs)) = L.HasMultipleValues @@ (x ': xs)

-- | Returns 'Just' the element before a given element in a list, if any.
--
-- Otherwise, returns 'Nothing'.
--
-- >>> :kind! ElemBefore 2 @@ (1 ':| '[2, 3])
-- ...
-- = Just 1
--
-- >>> :kind! ElemBefore 1 @@ (1 ':| '[2, 3])
-- ...
-- = Nothing
--
-- >>> :kind! ElemBefore 4 @@ (1 ':| '[2, 3])
-- ...
-- = Nothing
data ElemBefore :: a -> NonEmpty a -> Exp (Maybe a)

type instance Eval (ElemBefore x (y ':| '[])) = 'Nothing

type instance
  Eval (ElemBefore x (y ':| (z ': xs))) =
    Eval
      ( UnBool
          (L.ElemBefore x (z ': xs))
          (Pure ('Just y))
          =<< TyEq x z
      )

-- | Returns 'Just' the element after a given element in a list, if any.
--
-- Otherwise, returns 'Nothing'.
--
-- >>> :kind! ElemAfter 2 @@ (1 ':| '[2, 3])
-- ...
-- = Just 3
--
-- >>> :kind! ElemAfter 3 @@ (1 ':| '[2, 3])
-- ...
-- = Nothing
--
-- >>> :kind! ElemAfter 4 @@ (1 ':| '[2, 3])
-- ...
-- = Nothing
data ElemAfter :: a -> NonEmpty a -> Exp (Maybe a)

type instance Eval (ElemAfter x (y ':| '[])) = 'Nothing

type instance
  Eval (ElemAfter x (y ':| (z ': xs))) =
    Eval
      ( UnBool
          (L.ElemAfter x (z ': xs))
          (Pure ('Just z))
          =<< TyEq x y
      )
