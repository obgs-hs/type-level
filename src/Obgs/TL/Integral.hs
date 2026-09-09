{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : $Header$
-- Description : Representation of 'Integral' numbers that can be promoted to the type level.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- Representation of 'Integral' numbers that can be promoted to the type level.
module Obgs.TL.Integral
  ( -- * Types
    TypeIntegral,
    STypeIntegral (..),
    ValidIntegral,
    IntegralBound (..),
    BoundValue,

    -- * Class
    ToTypeIntegral (..),

    -- * Construction
    zero,
    plus,
    minus,
    Zero,
    Plus,
    PlusMaybe,
    Minus,
    MinusMaybe,

    -- * Getters
    NatValue,
    Inhabitants,
  )
where

import Data.Bifunctor (bimap)
import Data.Constraint (Constraint)
import Data.Eq.Singletons
import Data.Finitary
import Data.Finite (Finite)
import Data.Function (on)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.List (iterate')
import Data.Ord.Singletons
import Data.Singletons hiding (type (@@))
import Data.Singletons.Base.Enum (PEnum (Pred, Succ))
import Data.Singletons.TH (singEqInstance)
import Data.Word (Word16, Word32, Word64, Word8)
import Fcf (Eval, Exp, type (<=<), type (=<<), type (@@))
import Fcf qualified hiding (type (<=))
import Fcf.Utils qualified as Fcf
import GHC.Generics (Generic)
import GHC.TypeNats (Nat, natVal, type (+), type (-), type (<=?))
import Language.Haskell.TH.Lib (litT, numTyLit)
import Obgs.Fcf.Branch (ApplyWhen, Guard, type (-->))
import Obgs.Fcf.Combinators qualified as Fcf
import Obgs.Fcf.Constraint qualified as Fcf
import Obgs.Fcf.Enum qualified as Fcf
import Obgs.Fcf.Maybe qualified as Fcf
import Obgs.Fcf.Ord qualified as Fcf
import Obgs.TL.Error (ErrorMessage (..), TypeError, type (:$>:))
import Obgs.TL.Error qualified as TE
import Obgs.TL.Nat (Div, KnownNat)
import Obgs.TL.Type (type ($))
import Prelude.Singletons hiding (type ($), type (+), type (-), type (=<<), type (@@))
import Prelude.Singletons qualified as Sing
import Prelude

-- | An 'Integral' number that can be promoted to the type level.
--
-- At the term level, and for all intents and purposes,
-- it can be considered an isomorphism of its base 'Integral' type.
--
-- At the type level, one significant difference should be noted:
-- __no modular arithmetic is performed__.
--
-- Consequently, whenever the result of an operation would be out of bounds,
-- a 'TypeError' is thrown instead.
data TypeIntegral (a :: Type) where
  -- | A negative number.
  --
  -- Note that @Minus 0@ corresponds to @-1@
  -- (think of it as the zero-based index of the number on the negative scale).
  Minus :: !Nat -> TypeIntegral a
  -- | Zero.
  Zero :: TypeIntegral a
  -- | A positive number.
  --
  -- Note that @Plus 0@ corresponds to @+1@
  -- (think of it as the zero-based index of the number on the positive scale).
  Plus :: !Nat -> TypeIntegral a

deriving instance Eq (TypeIntegral a)

deriving instance Generic (TypeIntegral a)

instance Ord (TypeIntegral a) where
  compare = \case
    Minus n -> \case
      Minus m -> compare m n
      _ -> LT
    Zero -> \case
      Minus _ -> GT
      Zero -> EQ
      Plus _ -> LT
    Plus n -> \case
      Plus m -> compare n m
      _ -> GT

deriving instance Show (TypeIntegral a)

-- [Important note]
--
-- It is not guaranteed that the 'Nat' underlying a 'TypeIntegral' is within bounds,
-- due to the fact that most operations avoid bound checks for performance reasons.
-- This can be thanks to the specified property
-- that, for 'Integral' types from both the 'Data.Int' and 'Data.Word' modules:
-- "All arithmetic is performed modulo 2^n, where n is the number of bits in the type."
--
-- Consequently, operations that cannot rely on this property should apply bound checks beforehand.

-- | The base 'Data.Singletons.Sing' type for 'TypeIntegral's.
--
-- We have to roll our own instead of relying on 'Data.Singletons.TH',
-- otherwise the generated instances require type __@a@__ to be an instance of 'SingKind'.
data STypeIntegral (n :: TypeIntegral a) where
  SZero :: STypeIntegral 'Zero
  SPlus :: Sing n -> STypeIntegral ('Plus n)
  SMinus :: Sing n -> STypeIntegral ('Minus n)

deriving instance Show (STypeIntegral n)

type instance Sing = STypeIntegral

instance SingKind (TypeIntegral a) where
  type Demote (TypeIntegral a) = TypeIntegral a

  fromSing = \case
    SZero -> Zero
    SPlus n -> Plus $ fromSing n
    SMinus n -> Minus $ fromSing n

  toSing = \case
    Zero -> SomeSing SZero
    Plus n -> withSomeSing n $ SomeSing . SPlus
    Minus n -> withSomeSing n $ SomeSing . SMinus

instance SingI 'Zero where
  sing = SZero

instance (SingI n) => SingI ('Plus n) where
  sing = SPlus sing

instance (SingI n) => SingI ('Minus n) where
  sing = SMinus sing

$(singEqInstance ''TypeIntegral)

instance POrd (TypeIntegral a) where
  type Compare ('Minus m) ('Minus n) = Compare n m
  type Compare ('Minus _) 'Zero = 'LT
  type Compare ('Minus _) ('Plus _) = 'LT
  type Compare 'Zero ('Minus _) = 'GT
  type Compare 'Zero 'Zero = 'EQ
  type Compare 'Zero ('Plus _) = 'LT
  type Compare ('Plus _) ('Minus _) = 'GT
  type Compare ('Plus _) 'Zero = 'GT
  type Compare ('Plus m) ('Plus n) = Compare m n

-- When reviewing the instances below, keep in mind that @'Minus 0 = -1@, and @'Plus 0 = +1@.

$( [d|
     instance PBounded (TypeIntegral Int) where
       type MaxBound = 'Plus $(litT $ numTyLit $ fromIntegral $ maxBound @Int - 1)
       type MinBound = 'Minus $(litT $ numTyLit $ abs $ fromIntegral $ minBound @Int - 1)
     |]
 )

instance
  ( SingI (MinBound :: TypeIntegral Int),
    SingI (MaxBound :: TypeIntegral Int)
  ) =>
  SBounded (TypeIntegral Int)
  where
  sMinBound = Sing
  sMaxBound = Sing

instance PBounded (TypeIntegral Int8) where
  type MinBound = 'Minus 127
  type MaxBound = 'Plus 126

instance SBounded (TypeIntegral Int8) where
  sMinBound = Sing
  sMaxBound = Sing

instance PBounded (TypeIntegral Int16) where
  type MinBound = 'Minus 32767
  type MaxBound = 'Plus 32766

instance SBounded (TypeIntegral Int16) where
  sMinBound = Sing
  sMaxBound = Sing

instance PBounded (TypeIntegral Int32) where
  type MinBound = 'Minus 2147483647
  type MaxBound = 'Plus 2147483646

instance SBounded (TypeIntegral Int32) where
  sMinBound = Sing
  sMaxBound = Sing

instance PBounded (TypeIntegral Int64) where
  type MinBound = 'Minus 9223372036854775807
  type MaxBound = 'Plus 9223372036854775806

instance SBounded (TypeIntegral Int64) where
  sMinBound = Sing
  sMaxBound = Sing

$( [d|
     instance PBounded (TypeIntegral Word) where
       type MinBound = 'Zero
       type MaxBound = 'Plus $(litT $ numTyLit $ fromIntegral $ maxBound @Word - 1)
     |]
 )

instance
  ( SingI (MinBound :: TypeIntegral Word),
    SingI (MaxBound :: TypeIntegral Word)
  ) =>
  SBounded (TypeIntegral Word)
  where
  sMinBound = Sing
  sMaxBound = Sing

instance PBounded (TypeIntegral Word8) where
  type MinBound = 'Zero
  type MaxBound = 'Plus 254

instance SBounded (TypeIntegral Word8) where
  sMinBound = Sing
  sMaxBound = Sing

instance PBounded (TypeIntegral Word16) where
  type MinBound = 'Zero
  type MaxBound = 'Plus 65534

instance SBounded (TypeIntegral Word16) where
  sMinBound = Sing
  sMaxBound = Sing

instance PBounded (TypeIntegral Word32) where
  type MinBound = 'Zero
  type MaxBound = 'Plus 4294967294

instance SBounded (TypeIntegral Word32) where
  sMinBound = Sing
  sMaxBound = Sing

instance PBounded (TypeIntegral Word64) where
  type MinBound = 'Zero
  type MaxBound = 'Plus 18446744073709551614

instance SBounded (TypeIntegral Word64) where
  sMinBound = Sing
  sMaxBound = Sing

instance PBounded (TypeIntegral (Data.Finite.Finite n)) where
  type
    MinBound @(TypeIntegral (Data.Finite.Finite n)) =
      Fcf.UnBool
        (Fcf.Pure 'Zero)
        (Fcf.TError (TE.Text "Finite 0 is uninhabited"))
        @@ (n == 0)

  type
    MaxBound @(TypeIntegral (Finite n)) =
      Guard
        @@ '[ n == 0 --> Fcf.TError (TE.Text "Finite 0 is uninhabited"),
              n == 1 --> Fcf.Pure 'Zero,
              Otherwise --> Fcf.Pure ('Plus (n - 2))
            ]

instance
  ( SingI (MinBound :: TypeIntegral (Finite n)),
    SingI (MaxBound :: TypeIntegral (Finite n))
  ) =>
  SBounded (TypeIntegral (Finite n))
  where
  sMinBound = Sing
  sMaxBound = Sing

-- | A bound for a 'TypeIntegral' type.
data IntegralBound
  = Infinite
  | Finite Nat

-- | The underlying implementation of 'EnumFromThenTo' for 'TypeIntegral's,
-- given precalculated step and size.
data
  EnumFromThenTo' ::
    (TypeIntegral a -> TypeIntegral a -> Exp (TypeIntegral a)) ->
    TypeIntegral a ->
    Nat ->
    Nat ->
    Exp [TypeIntegral a]

type instance
  Eval (EnumFromThenTo' f from step size) =
    Fcf.IterateN (f (UnsafeFromNat @@ step)) from @@ (size `Div` step)

-- | Throws a 'TypeError' warning that an operation is not supported for unbounded types.
data ThrowUnsupportedOpForUnboundedTypeError :: Symbol -> Exp a

type instance
  Eval (ThrowUnsupportedOpForUnboundedTypeError op) =
    TypeError
      ( TE.Text "Unsupported operation for unbounded `Integral` types:"
          :$>: TE.Text op
      )

instance (PBounded (TypeIntegral a)) => PEnum (TypeIntegral a) where
  type Succ n = Eval (WithinBounds =<< UnsafeInc n)
  type Pred n = Eval (WithinBounds =<< UnsafeDec n)

  type
    ToEnum @(TypeIntegral a) n =
      Fcf.UnMaybe
        (ThrowUnsupportedOpForUnboundedTypeError "ToEnum")
        (WithinBounds <=< Fcf.Flip UnsafeAdd (UnsafeFromNat @@ n))
        @@ MinBoundMaybe a

  type
    FromEnum (n :: TypeIntegral a) =
      Fcf.UnMaybe
        (ThrowUnsupportedOpForUnboundedTypeError "FromEnum")
        (ToAbsNat <=< Fcf.Flip UnsafeSubtract n)
        @@ MinBoundMaybe a

  type
    EnumFromTo from to =
      Guard
        @@ '[ from > to --> Fcf.Pure '[],
              from == to --> Fcf.Pure '[from],
              Otherwise --> Fcf.IterateN UnsafeInc from (NatDiff from to)
            ]

  type
    EnumFromThenTo from then_ to =
      Guard
        @@ '[ (from < then_)
                --> If
                      (from > to)
                      (Fcf.Pure '[])
                      (EnumFromThenTo' UnsafeAdd from (NatDiff then_ from) (NatDiff to from)),
              (from > then_)
                --> If
                      (from < to)
                      (Fcf.Pure '[])
                      (EnumFromThenTo' UnsafeSubtract from (NatDiff from then_) (NatDiff from to)),
              Otherwise
                --> If
                      (from > to)
                      (Fcf.Pure '[])
                      (Fcf.ThrowInfiniteEnumError ShowNumber from then_ to)
            ]

instance PNum (TypeIntegral a) where
  type m + n = Eval (WithinBounds =<< UnsafeAdd m n)
  type m - n = Eval (WithinBounds =<< UnsafeSubtract n m)
  type m * n = Eval (WithinBounds =<< UnsafeMultiply m n)
  type Abs n = FromInteger (ToAbsNat @@ n)
  type Negate n = Eval (WithinBounds =<< UnsafeNegate n)
  type Signum 'Zero = 'Zero
  type Signum ('Plus _) = 'Plus 0
  type Signum ('Minus _) = 'Minus 0
  type FromInteger n = Eval (WithinBounds =<< UnsafeFromNat n)

-- | Maps the underlying 'Integral' number of a 'TypeIntegral'.
withIntegral :: (ToTypeIntegral a) => (a -> a) -> TypeIntegral a -> TypeIntegral a
withIntegral f = toTypeIntegral . f . toIntegral

-- | Maps the underlying 'Integral' number of a 'TypeIntegral' in some functorial context.
withIntegralF :: (Functor f, ToTypeIntegral a) => (a -> f a) -> TypeIntegral a -> f (TypeIntegral a)
withIntegralF f = fmap toTypeIntegral . f . toIntegral

-- | Applies a binary operation to the underlying 'Integral' numbers of two 'TypeIntegral's.
withIntegral2 ::
  (ToTypeIntegral a) =>
  (a -> a -> a) ->
  TypeIntegral a ->
  TypeIntegral a ->
  TypeIntegral a
withIntegral2 f = (toTypeIntegral .) . (f `on` toIntegral)

-- | Applies a binary operation resulting in a pair to the underlying 'Integral' numbers
-- of two 'TypeIntegral's.
withIntegralPair ::
  (ToTypeIntegral a) =>
  (a -> a -> (a, a)) ->
  TypeIntegral a ->
  TypeIntegral a ->
  (TypeIntegral a, TypeIntegral a)
withIntegralPair f = (bimap toTypeIntegral toTypeIntegral .) . (f `on` toIntegral)

-- | Increments a 'TypeIntegral'.
--
-- /This function is subject to overflow,/
-- /with the same behavior as the underlying 'Integral' type/.
unsafeIncrement :: TypeIntegral a -> TypeIntegral a
unsafeIncrement = \case
  Minus 0 -> Zero
  Minus n -> Minus $ n - 1
  Zero -> Plus 0
  Plus n -> Plus $ n + 1

-- | Adds a 'Nat' to a 'TypeIntegral'.
--
-- /This function is subject to overflow,/
-- /with the same behavior as the underlying 'Integral' type/.
unsafeAddNat :: Nat -> TypeIntegral a -> TypeIntegral a
unsafeAddNat n = \case
  Zero ->
    Plus $ n - 1
  Plus m ->
    Plus $ m + n
  Minus m ->
    if
      | m + 1 == n -> Zero
      | m >= n -> Minus $ m - n
      | otherwise -> Plus $ n - m - 2

-- | Subtracts a 'Nat' from a 'TypeIntegral'.
--
-- /This function is subject to overflow,/
-- /with the same behavior as the underlying 'Integral' type/.
unsafeSubtractNat :: Nat -> TypeIntegral a -> TypeIntegral a
unsafeSubtractNat n = \case
  Zero ->
    Minus $ n - 1
  Minus m ->
    Minus $ m + n
  Plus m ->
    if
      | m + 1 == n -> Zero
      | m >= n -> Plus $ m - n
      | otherwise -> Minus $ n - m - 2

-- | An unbounded variant of 'take'.
--
-- /The whole list is taken if the given 'Integer' is negative/.
unsafeTakeUB :: Integer -> [a] -> [a]
unsafeTakeUB 0 _ = []
unsafeTakeUB _ [] = []
unsafeTakeUB n (x : xs) = x : unsafeTakeUB (n - 1) xs

instance (Bounded a, ToTypeIntegral a) => Bounded (TypeIntegral a) where
  minBound = toTypeIntegral @a minBound
  maxBound = toTypeIntegral @a maxBound

instance (Enum a, Integral a, ToTypeIntegral a) => Enum (TypeIntegral a) where
  succ = withIntegral succ
  pred = withIntegral pred
  toEnum = toTypeIntegral . toEnum @a
  fromEnum = fromEnum . toIntegral

  enumFrom from = case maxBoundMaybe @a of
    Nothing -> iterate' unsafeIncrement from
    Just maxBound' -> enumFromTo from maxBound'

  enumFromTo from to
    | from > to =
        []
    | from == to =
        [from]
    | otherwise =
        let size = fromIntegral to - fromIntegral from + 1
         in unsafeTakeUB size $ iterate' unsafeIncrement from

  enumFromThenTo from then_ to
    | from < then_ =
        if from > to
          then []
          else -- from <= to
            let diff = fromIntegral to - fromIntegral from
                step = fromIntegral then_ - fromIntegral from
                count = diff `div` step + 1
             in unsafeTakeUB count $ iterate' (unsafeAddNat $ fromInteger step) from
    | from > then_ =
        if from < to
          then []
          else -- from >= to
            let diff = fromIntegral from - fromIntegral to
                step = fromIntegral from - fromIntegral then_
                count = diff `div` step + 1
             in unsafeTakeUB count $ iterate' (unsafeSubtractNat $ fromInteger step) from
    | otherwise =
        if from > to
          then []
          else repeat from

instance (Finitary a, ToTypeIntegral a) => Finitary (TypeIntegral a) where
  type Cardinality (TypeIntegral a) = Cardinality a
  fromFinite = toTypeIntegral . fromFinite
  toFinite = toFinite . toIntegral
  start = toTypeIntegral start
  end = toTypeIntegral end
  previous = withIntegralF previous
  next = withIntegralF next

-- | Negates a 'TypeIntegral'.
--
-- /Does not check whether the result is within bounds/.
data UnsafeNegate :: TypeIntegral a -> Exp (TypeIntegral a)

type instance Eval (UnsafeNegate 'Zero) = 'Zero

type instance Eval (UnsafeNegate ('Minus n)) = 'Plus n

type instance Eval (UnsafeNegate ('Plus n)) = 'Minus n

-- | Increments a 'TypeIntegral'.
--
-- /Does not check whether the result is within bounds/.
data UnsafeInc :: TypeIntegral a -> Exp (TypeIntegral a)

type instance Eval (UnsafeInc 'Zero) = 'Plus 0

type instance Eval (UnsafeInc ('Plus n)) = 'Plus $ n + 1

type instance Eval (UnsafeInc ('Minus n)) = If (n == 0) 'Zero ('Minus $ n - 1)

-- | Decrements a 'TypeIntegral'.
--
-- /Does not check whether the result is within bounds/.
data UnsafeDec :: TypeIntegral a -> Exp (TypeIntegral a)

type instance Eval (UnsafeDec 'Zero) = 'Minus 0

type instance Eval (UnsafeDec ('Minus n)) = 'Minus $ n + 1

type instance Eval (UnsafeDec ('Plus n)) = If (n == 0) 'Zero ('Plus $ n - 1)

-- | Adds two 'TypeIntegral's.
--
-- /Does not check whether the result is within bounds/.
data UnsafeAdd :: TypeIntegral a -> TypeIntegral a -> Exp (TypeIntegral a)

type instance Eval (UnsafeAdd n 'Zero) = n

type instance Eval (UnsafeAdd 'Zero n) = n

type instance Eval (UnsafeAdd ('Plus m) ('Plus n)) = 'Plus $ m + n + 1

type instance Eval (UnsafeAdd ('Minus m) ('Minus n)) = 'Minus $ m + n + 1

type instance Eval (UnsafeAdd ('Plus m) ('Minus n)) = Eval (UnsafeAdd ('Minus n) ('Plus m))

type instance
  Eval (UnsafeAdd ('Minus m) ('Plus n)) =
    Guard
      @@ '[ m == n --> Fcf.Pure 'Zero,
            m <=? n --> Fcf.Pure ('Plus (n - m - 1)),
            Otherwise --> Fcf.Pure ('Minus (m - n - 1))
          ]

-- | Subtracts a 'TypeIntegral' from another.
--
-- /Does not check whether the result is within bounds/.
data UnsafeSubtract :: TypeIntegral a -> TypeIntegral a -> Exp (TypeIntegral a)

type instance Eval (UnsafeSubtract x y) = Eval (UnsafeAdd y =<< UnsafeNegate x)

-- | Multiplies two 'TypeIntegral's.
--
-- /Does not check whether the result is within bounds/.
data UnsafeMultiply :: TypeIntegral a -> TypeIntegral a -> Exp (TypeIntegral a)

type instance Eval (UnsafeMultiply 'Zero _) = 'Zero

type instance Eval (UnsafeMultiply _ 'Zero) = 'Zero

type instance Eval (UnsafeMultiply ('Minus m) ('Minus n)) = Eval (UnsafeMultiply ('Plus m) ('Plus n))

type instance Eval (UnsafeMultiply ('Plus m) ('Plus n)) = 'Plus $ UnsafeMultiply' m n

type instance Eval (UnsafeMultiply ('Minus m) ('Plus n)) = 'Minus $ UnsafeMultiply' m n

type instance Eval (UnsafeMultiply ('Plus m) ('Minus n)) = 'Minus $ UnsafeMultiply' m n

-- | The base case for 'UnsafeMultiply'.
type UnsafeMultiply' (m :: Nat) (n :: Nat) = (m + 1) * (n + 1) - 1

-- | Converts a 'Nat' to a 'TypeIntegral'.
--
-- /Does not check whether the result is within bounds/.
data UnsafeFromNat :: Nat -> Exp (TypeIntegral a)

type instance Eval (UnsafeFromNat n) = If (n == 0) 'Zero ('Plus $ n - 1)

-- | Returns the absolute 'Nat' value of a 'TypeIntegral'.
data ToAbsNat :: TypeIntegral a -> Exp Nat

type instance Eval (ToAbsNat 'Zero) = 0

type instance Eval (ToAbsNat ('Minus n)) = n + 1

type instance Eval (ToAbsNat ('Plus n)) = n + 1

-- | Returns the 'Nat' difference between two 'TypeIntegral's.
type family NatDiff (m :: TypeIntegral a) (n :: TypeIntegral a) :: Nat where
  NatDiff 'Zero 'Zero = 0
  NatDiff 'Zero ('Minus n) = n + 1
  NatDiff 'Zero ('Plus n) = n + 1
  NatDiff ('Minus m) 'Zero = m + 1
  NatDiff ('Minus m) ('Minus n) = If (m > n) (m - n) (n - m)
  NatDiff ('Minus m) ('Plus n) = m + n + 2
  NatDiff ('Plus m) 'Zero = m + 1
  NatDiff ('Plus m) ('Plus n) = If (m > n) (m - n) (n - m)
  NatDiff ('Plus m) ('Minus n) = m + n + 2

-- | Shows a 'TypeIntegral' for use in an 'ErrorMessage'.
data ShowNumber :: TypeIntegral a -> Exp ErrorMessage

type instance Eval (ShowNumber 'Zero) = TE.Text "0"

type instance Eval (ShowNumber ('Minus n)) = TE.Text "-" :<>: TE.ShowType (n + 1)

type instance Eval (ShowNumber ('Plus n)) = TE.ShowType (n + 1)

-- | Shows an 'IntegralBound' for use in an 'ErrorMessage'.
data ShowBound :: Symbol -> IntegralBound -> Exp ErrorMessage

type instance
  Eval (ShowBound sign 'Infinite) =
    TE.Text sign :<>: TE.Text "Inf."

type instance
  Eval (ShowBound sign ('Finite n)) =
    ApplyWhen (n /= 0) (Fcf.Pure1 ((:<>:) (TE.Text sign))) @@ TE.ShowType n

-- | Shows the range of values of a 'TypeIntegral' base type, for use in an 'ErrorMessage'.
data ShowTypeRange :: a -> Exp ErrorMessage

type instance
  Eval (ShowTypeRange a) =
    (Fcf.UnMaybe (Fcf.Pure $ TE.Text "0") (ShowBound "-") @@ MinusBound a)
      :<>: TE.Text " `To` "
      :<>: (Fcf.UnMaybe (Fcf.Pure $ TE.Text "0") (ShowBound "") @@ PlusBound a)

-- | Throws a 'TypeError' warning that a 'TypeIntegral' is out of bounds.
data ThrowNumberOutOfBoundsError :: TypeIntegral a -> Exp b

type instance
  Eval (ThrowNumberOutOfBoundsError (n :: TypeIntegral a)) =
    TypeError
      ( TE.Text "Number:"
          :$>: (ShowNumber @@ n)
          :$$: TE.Text "is not contained in "
          :<>: TE.ShowTypeQuoted a
          :<>: TE.Text " range:"
          :$>: (ShowTypeRange @@ a)
      )

-- | Returns 'Just' the given 'TypeIntegral' if it is within bounds, otherwise 'Nothing'.
data WithinBoundsMaybe :: TypeIntegral a -> Exp (Maybe (TypeIntegral a))

type instance
  Eval (WithinBoundsMaybe (n :: TypeIntegral a)) =
    If
      ( Fcf.IsBetween
          (Fcf.FromMaybe n @@ MinBoundMaybe a)
          (Fcf.FromMaybe n @@ MaxBoundMaybe a)
          @@ n
      )
      ('Just n)
      'Nothing

-- | Returns the given 'TypeIntegral' if it is within bounds, otherwise throws a 'TypeError'.
data WithinBounds :: TypeIntegral a -> Exp (TypeIntegral a)

type instance
  Eval (WithinBounds n) =
    Eval
      ( Fcf.FromMaybeM (ThrowNumberOutOfBoundsError n)
          =<< WithinBoundsMaybe n
      )

-- | Requires that a 'TypeIntegral' is within bounds.
data ValidIntegral :: TypeIntegral a -> Exp Constraint

type instance
  Eval (ValidIntegral n) =
    Fcf.UnlessC
      ((MinBound <= n) && (n <= MaxBound))
      @@ ThrowNumberOutOfBoundsError n

-- | Returns the value of an 'IntegralBound', or 'Nothing' if it is 'Infinite'.
data BoundValue :: IntegralBound -> Exp (Maybe Nat)

type instance Eval (BoundValue 'Infinite) = 'Nothing

type instance Eval (BoundValue ('Finite n)) = 'Just n

-- | Returns the appropriate 'Data.Finite.Finite' 'IntegralBound' for a given 'Nat'.
type family ToFiniteBound (n :: Nat) :: Maybe IntegralBound where
  ToFiniteBound 0 = 'Nothing
  ToFiniteBound n = 'Just ('Finite n)

-- | A common interface for 'Integral' types that can be promoted to the type level
-- through 'TypeIntegral's.
class (Integral a) => ToTypeIntegral a where
  -- | 'Just' 'MinBound' for 'Bounded' 'Integral's, otherwise 'Nothing'.
  type MinBoundMaybe a :: Maybe (TypeIntegral a)

  type MinBoundMaybe a = 'Just (MinBound :: TypeIntegral a)

  -- | 'Just' 'MaxBound' for 'Bounded' 'Integral's, otherwise 'Nothing'.
  type MaxBoundMaybe a :: Maybe (TypeIntegral a)

  type MaxBoundMaybe a = 'Just (MaxBound :: TypeIntegral a)

  -- | 'Just' the 'IntegralBound' for positive numbers,
  -- or 'Nothing' if the type does not support positive numbers.
  type PlusBound a :: Maybe IntegralBound

  type PlusBound a = ToFiniteBound (ToAbsNat @@ (MaxBound :: TypeIntegral a))

  -- | 'Just' the 'IntegralBound' for negative numbers,
  -- or 'Nothing' if the type does not support negative numbers.
  type MinusBound a :: Maybe IntegralBound

  type MinusBound a = ToFiniteBound (ToAbsNat @@ (MinBound :: TypeIntegral a))

  -- | Returns 'Just' 'minBound' for 'Bounded' types, or 'Nothing' if the type is unbounded.
  minBoundMaybe :: Maybe (TypeIntegral a)

  -- | Returns 'Just' 'maxBound' for 'Bounded' types, or 'Nothing' if the type is unbounded.
  maxBoundMaybe :: Maybe (TypeIntegral a)

  -- | Converts an 'Integral' number to the corresponding 'TypeIntegral'.
  toTypeIntegral :: a -> TypeIntegral a

  -- | Converts a 'TypeIntegral' number to the corresponding 'Integral'.
  toIntegral :: TypeIntegral a -> a

  default minBoundMaybe :: (Bounded a) => Maybe (TypeIntegral a)
  minBoundMaybe = Just $ toTypeIntegral $ minBound @a

  default maxBoundMaybe :: (Bounded a) => Maybe (TypeIntegral a)
  maxBoundMaybe = Just $ toTypeIntegral $ maxBound @a

  default toTypeIntegral :: (Integral a) => a -> TypeIntegral a
  toTypeIntegral n
    | n < 0 = Minus $ fromIntegral $ abs n - 1
    | n > 0 = Plus $ fromIntegral n - 1
    | otherwise = Zero

  default toIntegral :: (Integral a) => TypeIntegral a -> a
  toIntegral = \case
    Zero -> 0
    Plus n -> fromIntegral n + 1
    Minus n -> negate $ fromIntegral n + 1

instance ToTypeIntegral Integer where
  type MinBoundMaybe _ = Nothing
  type MaxBoundMaybe _ = Nothing
  type PlusBound _ = 'Just 'Infinite
  type MinusBound _ = 'Just 'Infinite
  minBoundMaybe = Nothing
  maxBoundMaybe = Nothing

instance ToTypeIntegral Int

instance ToTypeIntegral Int8

instance ToTypeIntegral Int16

instance ToTypeIntegral Int32

instance ToTypeIntegral Int64

instance ToTypeIntegral Word

instance ToTypeIntegral Word8

instance ToTypeIntegral Word16

instance ToTypeIntegral Word32

instance ToTypeIntegral Word64

instance (KnownNat n) => ToTypeIntegral (Finite n) where
  -- 'fromIntegral', unlike the arithmetic operations, does not perform modular arithmetic.
  toIntegral = \case
    Minus m -> fromIntegral $ n - m `mod` n - 1
    Zero -> 0
    Plus m -> fromIntegral $ (m + 1) `mod` n
    where
      n = natVal (Proxy @n)

-- | Converts a 'Nat' to the corresponding zero-based index.
type family ToNonZeroIx (n :: Nat) :: Nat where
  ToNonZeroIx 0 = 0
  ToNonZeroIx n = n - 1

-- | Returns a 'Data.Proxy.Proxy' for a null 'TypeIntegral'.
zero :: Proxy ('Zero :: TypeIntegral a)
zero = Proxy

-- | Returns a 'Data.Proxy.Proxy' for a positive 'TypeIntegral'.
--
-- Throws a 'TypeError' if the result is out of bounds.
plus ::
  forall (n :: Nat) a (i :: TypeIntegral a).
  ( ToTypeIntegral a,
    ValidIntegral @@ i,
    i ~ 'Plus (ToNonZeroIx n)
  ) =>
  Proxy i
plus = Proxy

-- | Returns a 'Data.Proxy.Proxy' for a negative 'TypeIntegral'.
--
-- Throws a 'TypeError' if the result is out of bounds.
minus ::
  forall (n :: Nat) a (i :: TypeIntegral a).
  ( ToTypeIntegral a,
    ValidIntegral @@ i,
    i ~ 'Minus (ToNonZeroIx n)
  ) =>
  Proxy i
minus = Proxy

instance (ToTypeIntegral a) => Num (TypeIntegral a) where
  x + y =
    case (x, y) of
      (Zero, _) -> y
      (_, Zero) -> x
      (Minus m, Minus n) -> Minus $ m + n + 1
      (Plus m, Plus n) -> Plus $ m + n + 1
      (Minus _, Plus _) -> y + x
      (Plus m, Minus n) ->
        if
          | m == n -> Zero
          | m > n -> Plus $ m - n - 1
          | otherwise -> Minus $ n - m - 1

  x - y = x + negate y

  x * y =
    case (x, y) of
      (Zero, _) -> Zero
      (_, Zero) -> Zero
      (Minus m, Minus n) -> Plus m * Plus n
      (Plus m, Plus n) -> Plus $ go m n
      (Minus m, Plus n) -> Minus $ go m n
      (Plus m, Minus n) -> Minus $ go m n
    where
      go :: Nat -> Nat -> Nat
      go m n = (m + 1) * (n + 1) - 1

  negate = \case
    Zero -> Zero
    Plus n -> Minus n
    Minus n -> Plus n

  abs = \case
    Zero -> Zero
    Plus n -> Plus n
    Minus n -> Plus n

  signum = \case
    Zero -> Zero
    Plus _ -> Plus 0
    Minus _ -> Minus 0

  -- Use 'fromInteger' to account for arithmetic exceptions that may be thrown
  -- by the 'Integral' instance.
  fromInteger = toTypeIntegral . fromInteger

instance (ToTypeIntegral a) => Real (TypeIntegral a) where
  toRational = toRational . toIntegral

instance (ToTypeIntegral a, Enum (TypeIntegral a)) => Integral (TypeIntegral a) where
  quot = withIntegral2 quot
  rem = withIntegral2 rem
  div = withIntegral2 div
  mod = withIntegral2 mod
  quotRem = withIntegralPair quotRem
  divMod = withIntegralPair divMod
  toInteger = toInteger . toIntegral

-- | Returns a negative 'TypeIntegral'.
--
-- Throws a 'TypeError' if the result is out of bounds.
type family Minus (n :: Nat) :: TypeIntegral a where
  Minus 0 = 'Zero
  Minus n = WithinBounds @@ 'Minus (ToNonZeroIx n)

-- | Returns a null 'TypeIntegral'.
type family Zero :: TypeIntegral a where
  Zero = 'Zero

-- | Returns a positive 'TypeIntegral'.
--
-- Throws a 'TypeError' if the result is out of bounds.
type family Plus (n :: Nat) :: TypeIntegral a where
  Plus 0 = 'Zero
  Plus n = WithinBounds @@ 'Plus (ToNonZeroIx n)

-- | Returns 'Just' a negative 'TypeIntegral', or 'Nothing' if the result is out of bounds.
type family MinusMaybe (n :: Nat) :: Maybe (TypeIntegral a) where
  MinusMaybe 0 = 'Just 'Zero
  MinusMaybe n = WithinBoundsMaybe @@ 'Minus (ToNonZeroIx n)

-- | Returns 'Just' a positive 'TypeIntegral', or 'Nothing' if the result is out of bounds.
type family PlusMaybe (n :: Nat) :: Maybe (TypeIntegral a) where
  PlusMaybe 0 = 'Just 'Zero
  PlusMaybe n = WithinBoundsMaybe @@ 'Plus (ToNonZeroIx n)

-- | Returns all possible 'TypeIntegral's for type __@a@__.
type family Inhabitants (a :: Type) :: [TypeIntegral a] where
  Inhabitants a =
     Fcf.IterateN UnsafeInc (MinBound @(TypeIntegral a))
       @@ (Cardinality (TypeIntegral a) - 1)

-- | Returns the absolute 'Nat' value of a 'TypeIntegral'.
type family NatValue (n :: TypeIntegral a) :: Nat where
  NatValue ('Minus n) = n + 1
  NatValue 'Zero = 0
  NatValue ('Plus n) = n + 1