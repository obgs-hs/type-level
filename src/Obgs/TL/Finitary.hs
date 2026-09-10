{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : Type-level variant of 'Data.Finitary.Finitary'.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- Type-level variant of 'Data.Finitary.Finitary'.
module Obgs.TL.Finitary
  ( -- * Class
    PFinitary (..),

    -- * Indices
    Index,
    ToIndex,
    ToIndexMaybe,
    FromIndex,
    Indices,

    -- * Re-exports
    Cardinality,
  )
where

import Data.Finitary
import Data.Finite (Finite)
import Data.Kind (Type)
import Data.Singletons.Base.Enum (PBounded (..), PEnum (..))
import Fcf (type (@@))
import Fcf qualified
import Obgs.Fcf.Enum qualified as Fcf
import Obgs.TL.Integral (TypeIntegral)
import Obgs.TL.Integral qualified as TI
import Obgs.TL.List (Map)
import Obgs.TL.Maybe (MapJust)
import Obgs.TL.Nat (Nat)
import Obgs.TL.Tuple ()
import Prelude

-- | The index of some value of type __@a@__, between @0@ and @'Cardinality' a - 1@.
type data Index (a :: Type) where
  Ix :: TypeIntegral (Finite (Cardinality a)) -> Index a

instance PBounded (Index a) where
  type MinBound @(Index a) = Ix MinBound
  type MaxBound @(Index a) = Ix MaxBound

instance PEnum (Index a) where
  type Succ (Ix n) = Ix (Succ n)
  type Pred (Ix n) = Ix (Pred n)
  type FromEnum (Ix n) = FromEnum n
  type ToEnum n = Ix (ToEnum n)
  type EnumFromTo (Ix from) (Ix to) = Map Ix (EnumFromTo from to)
  type EnumFromThenTo (Ix from) (Ix then_) (Ix to) = Map Ix (EnumFromThenTo from then_ to)

-- | Converts a 'Nat' to the corresponding 'Index'.
--
-- Throws a 'Obgs.TL.Error.TypeError' if the 'Nat' is out of bounds.
type family ToIndex (n :: Nat) :: Index a where
  ToIndex n = Ix (TI.Plus n)

-- | Converts a 'Nat' to the corresponding 'Index'.
--
-- Returns 'Nothing' if the 'Nat' is out of bounds.
type family ToIndexMaybe (n :: Nat) :: Maybe (Index a) where
  ToIndexMaybe n = MapJust Ix (TI.PlusMaybe n)

-- | Converts an 'Index' to the corresponding 'Nat'.
type family FromIndex (ix :: Index a) :: Nat where
  FromIndex (Ix n) = TI.NatValue n

-- | A list of all indices corresponding to 'Inhabitants' of type __@a@__.
type Indices a =
  Fcf.Map (Fcf.Pure1 Ix)
    @@ (Inhabitants @(TypeIntegral (Finite (Cardinality a)))) ::
    [Index a]

-- | A @singletons@-like type-level variant of 'Data.Finitary.Finitary'.
--
-- /Default implementations of each associated type family expect type __@a@__/
-- /to have instances of 'PBounded' and\/or 'PEnum',/
-- /and 'FromEnum' indices to be __0-based__ and __monotonically increasing__./
class (Finitary a) => PFinitary (a :: Type) where
  -- | A type-level variant of 'Data.Finitary.start'.
  type Start :: a

  type Start = MinBound

  -- | A type-level variant of 'Data.Finitary.end'.
  type End :: a

  type End = MaxBound

  -- | A type-level variant of 'Data.Finitary.previous'.
  type Previous (x :: a) :: Maybe a

  type Previous x = Fcf.PredMaybe @@ x

  -- | A type-level variant of 'Data.Finitary.next'.
  type Next (x :: a) :: Maybe a

  type Next x = Fcf.SuccMaybe @@ x

  -- | A type-level variant of 'Data.Finitary.fromIndex'.
  type FromFinite (n :: Index a) :: a

  type FromFinite n = ToEnum (FromIndex n)

  -- | A type-level variant of 'Data.Finitary.toIndex'.
  type ToFinite (x :: a) :: Index a

  type ToFinite x = ToIndex (FromEnum x)

  -- | A type-level variant of 'Data.Finitary.inhabitants'.
  type Inhabitants :: [a]

  type Inhabitants = InhabitantsFromTo Start End

  -- | A type-level variant of 'Data.Finitary.inhabitantsFrom'.
  type InhabitantsFrom (from :: a) :: [a]

  type InhabitantsFrom from = InhabitantsFromTo from End

  -- | A type-level variant of 'Data.Finitary.inhabitantsTo'.
  type InhabitantsTo (to :: a) :: [a]

  type InhabitantsTo to = InhabitantsFromTo Start to

  -- | A type-level variant of 'Data.Finitary.inhabitantsFromTo'.
  type InhabitantsFromTo (from :: a) (to :: a) :: [a]

  type InhabitantsFromTo from to = EnumFromTo from to

instance PFinitary ()

instance PFinitary Bool

instance PFinitary Char

instance (PFinitary a) => PFinitary (Maybe a) where
  type Start = 'Nothing

  type End = 'Just End

  type Previous 'Nothing = 'Nothing
  type Previous ('Just x) = 'Just (Previous x)

  type Next 'Nothing = 'Just ('Just Start)
  type Next ('Just x) = MapJust 'Just (Next x)

  type Inhabitants = 'Nothing ': Map 'Just Inhabitants

  type InhabitantsFrom 'Nothing = Inhabitants
  type InhabitantsFrom ('Just x) = Map 'Just (InhabitantsFrom x)

  type InhabitantsTo 'Nothing = '[ 'Nothing]
  type InhabitantsTo ('Just to) = 'Nothing ': Map 'Just (InhabitantsTo to)

  type InhabitantsFromTo 'Nothing to = InhabitantsTo to
  type InhabitantsFromTo ('Just from) 'Nothing = '[]
  type InhabitantsFromTo ('Just from) ('Just to) = Map 'Just (InhabitantsFromTo from to)

instance (PFinitary a, PFinitary b) => PFinitary (a, b)

instance
  ( Finitary a,
    TI.ToTypeIntegral a,
    PEnum (TypeIntegral a),
    PBounded (TypeIntegral a)
  ) =>
  PFinitary (TypeIntegral a)
  where
  type Inhabitants @(TypeIntegral a) = TI.Inhabitants a