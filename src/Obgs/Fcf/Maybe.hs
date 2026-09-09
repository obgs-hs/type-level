{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : First-class families for working with 'Maybe' values.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for working with 'Maybe' values.
module Obgs.Fcf.Maybe
  ( -- * Type-level variants
    FromMaybeM,
    CatMaybes,

    -- * Combinator variants
    type (<$?>),
    type (<?&>),
    type (?=<<),
    type (<?=<),

    -- * Lists
    type (?:),
    type (^?:),
    type (^?:^),
  )
where

import Fcf (Eval, Exp, LiftM2, Pure, Pure1, UnMaybe, type (=<<))
import Fcf.Class.Functor (FMap)
import Obgs.Fcf.Combinators (type (<&>), type (<.>))
import Prelude (Maybe (..))

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
-- >>> :m +Obgs.Fcf.Enum
--

-- | A variant of 'Fcf.FromMaybe' with a lifted default value.
--
-- >>> :kind! Eval (FromMaybeM (Pure 0) ('Just 1))
-- ...
-- = 1
--
-- >>> :kind! Eval (FromMaybeM (Pure 0) 'Nothing)
-- ...
-- = 0
data FromMaybeM :: Exp a -> Maybe a -> Exp a

type instance Eval (FromMaybeM z ('Just a)) = a

type instance Eval (FromMaybeM z 'Nothing) = Eval z

-- | A type-level variant of 'Data.Maybe.catMaybes'.
--
-- >>> :kind! Eval (CatMaybes '[ 'Just 1, 'Nothing, 'Just 2 ])
-- ...
-- = [1, 2]
data CatMaybes :: [Maybe a] -> Exp [a]

type instance Eval (CatMaybes '[]) = '[]

type instance Eval (CatMaybes ('Nothing ': xs)) = Eval (CatMaybes xs)

type instance Eval (CatMaybes ('Just x ': xs)) = x ': Eval (CatMaybes xs)

-- | A variant of t'Fcf.<$>' for dealing with 'Maybe'-based 'Exp's.
--
-- >>> :kind! Eval ('(,) 1 <$?> Pure ('Just 2))
-- ...
-- = Just '(1, 2)
data (<$?>) :: (a -> b) -> Exp (Maybe a) -> Exp (Maybe b)

type instance Eval (f <$?> a) = Eval (FMap (Pure1 f) =<< a)

infixl 4 <$?>

-- | A variant of t'Obgs.Fcf.Combinators.<&>' for dealing with 'Maybe'-based 'Exp's.
--
-- >>> :kind! Eval (Pure ('Just 'Right) <?&> 1)
-- ...
-- = Just (Right 1)
--
-- >>> :kind! Eval (Pure 'Nothing <?&> 1)
-- ...
-- = Nothing
data (<?&>) :: Exp (Maybe (a -> b)) -> a -> Exp (Maybe b)

type instance
  Eval (f <?&> a) =
    Eval ('Just <.> (<&>) (Pure a) ?=<< f)

infixl 4 <?&>

-- | A variant of t'Fcf.=<<' for dealing with 'Maybe'-based 'Exp's.
--
-- >>> :kind! Eval (SuccMaybe ?=<< SuccMaybe 'a')
-- ...
-- = Just 'c'
data (?=<<) :: (a -> Exp (Maybe b)) -> Exp (Maybe a) -> Exp (Maybe b)

type instance Eval (f ?=<< a) = Eval (UnMaybe (Pure 'Nothing) f =<< a)

infixr 1 ?=<<

-- | A variant of t'Fcf.<=<' for dealing with 'Maybe'-based 'Exp's.
--
-- >>> :kind! Eval ((SuccMaybe <?=< SuccMaybe) 'a')
-- ...
-- = Just 'c'
data (<?=<) :: (b -> Exp (Maybe c)) -> (a -> Exp (Maybe b)) -> a -> Exp (Maybe c)

type instance Eval ((<?=<) g f a) = Eval (g ?=<< f a)

infixr 1 <?=<

-- | Prepends 'Just' an element to a list.
--
-- >>> :kind! Eval ('Just 1 ?: '[2, 3])
-- ...
-- = [1, 2, 3]
--
-- >>> :kind! Eval ('Nothing ?: '[2, 3])
-- ...
-- = [2, 3]
data (?:) :: Maybe a -> [a] -> Exp [a]

type instance Eval ('Nothing ?: as) = as

type instance Eval ('Just a ?: as) = a ': as

infixr 5 ?:

-- | A semi-lifted variant of @(?:)@.
data (^?:) :: Exp (Maybe a) -> [a] -> Exp [a]

type instance Eval (a ^?: as) = Eval (Eval a ?: as)

infixr 5 ^?:

-- | A lifted variant of @(?:)@.
data (^?:^) :: Exp (Maybe a) -> Exp [a] -> Exp [a]

type instance Eval (a ^?:^ as) = Eval (LiftM2 (?:) a as)

infixr 5 ^?:^