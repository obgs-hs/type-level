{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : $Header$
-- Description : Type utilities for working with 'Maybe' values.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- Type utilities for working with 'Maybe' values.
--
-- This module also introduces orphan 'PBounded' and 'PEnum' instances for 'Maybe'.
module Obgs.TL.Maybe
  ( -- * Type-level variants
    FromMaybe,
    type (<|>),

    -- * Combinators
    type (??),

    -- * Mapping
    MapJust,

    -- * Lists
    type (?:),
  )
where

import Data.Singletons.Base.Enum (PBounded (..), PEnum (..))
import Data.Type.Bool (If)
import Fcf (Eval)
import Obgs.Fcf.Enum qualified as Fcf
import Obgs.TL.List (Map)
import Obgs.TL.Nat (type (+), type (-))
import Obgs.TL.Type (type (==))

-- | A type-level variant of 'Data.Maybe.fromMaybe'.
type family FromMaybe (z :: a) (x :: Maybe a) :: a where
  FromMaybe z 'Nothing = z
  FromMaybe _ ('Just x) = x

-- | A type-level variant of 'Control.Applicative.<|>'.
type family (a :: Maybe k) <|> (b :: Maybe k) :: Maybe k where
  'Nothing <|> y = y
  ('Just x) <|> _ = 'Just x

-- | Maps a function over a 'Maybe' value.
type family MapJust (f :: a -> b) (x :: Maybe a) :: Maybe b where
  MapJust _ 'Nothing = 'Nothing
  MapJust f ('Just x) = 'Just (f x)

-- | Prepends 'Just' an element to a list.
--
-- >>> :kind! 'Just 1 ?: '[2, 3]
-- ...
-- = [1, 2, 3]
--
-- >>> :kind! 'Nothing ?: '[2, 3]
-- ...
-- = [2, 3]
type family (?:) (a :: Maybe k) (as :: [k]) :: [k] where
  'Just x ?: xs = x ': xs
  'Nothing ?: xs = xs

infixr 5 ?:

instance (PBounded a) => PBounded (Maybe a) where
  type MinBound = 'Nothing
  type MaxBound = 'Just MaxBound

instance (PEnum a) => PEnum (Maybe a) where
  type FromEnum 'Nothing = 0
  type FromEnum ('Just x) = 1 + FromEnum x

  type
    ToEnum n =
      If
        (n == 0)
        'Nothing
        ('Just (ToEnum (n - 1)))

  type
    EnumFromTo 'Nothing 'Nothing =
      '[ 'Nothing]
  type
    EnumFromTo 'Nothing ('Just to) =
      'Nothing ': Map 'Just (EnumFromTo MinBound to)
  type
    EnumFromTo ('Just from) 'Nothing =
      '[]
  type
    EnumFromTo ('Just from) ('Just to) =
      Map 'Just (EnumFromTo from to)

  type
    EnumFromThenTo from then_ to =
      Eval (Fcf.EnumFromThenToByIx 'Nothing 'Nothing from then_ to)

-- | An operator variant of 'FromMaybe' with flipped arguments.
--
-- >>> :kind! 'Nothing ?? 3
-- ...
-- = 3
--
-- >>> :kind! 'Just 1 ?? 'Nothing ?? 'Just 2 ?? 3
-- ...
-- = 1
type family (??) (a :: Maybe k) (b :: k) :: k where
  'Nothing ?? x = x
  ('Just x) ?? _ = x

infixr 0 ??