{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : $Header$
-- Description : First-class families for working with 'Enum' types.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for working with 'Enum' types.
--
-- /Most of the families in this module expect the types involved to have 'PBounded' and\/or 'PEnum' instances/.
module Obgs.Fcf.Enum
  ( -- * Type-level variants
    MinBound,
    MaxBound,
    Succ,
    Pred,
    FromEnum,
    ToEnum,
    EnumFromTo,
    EnumFromThenTo,

    -- * Enumeration
    SuccMaybe,
    PredMaybe,

    -- * Predicates
    IsMultipleEnum,

    -- * Implementation helpers
    EnumFromToByIx,
    EnumFromThenToByIx,
    EnumFromThenToByCase,
    NatEnumFromThenTo,
    ThrowInfiniteEnumError,

    -- * Testing
    testBoundedEnumInstance,
    testEnumInstance,
  )
where

import Data.Finitary qualified as Fin
import Data.Proxy (Proxy)
import Data.Singletons (SingI, SingKind (..), demote)
import Data.Kind (Type)
import Data.Singletons qualified as Sing
import Data.Singletons.Base.Enum (PBounded, PEnum)
import Data.Singletons.Base.Enum qualified as Sing
import Fcf (Eval, Exp, FromMaybe, Map, Not, Pure, TyEq, UnMaybe, type (<$>), type (=<<), type (@@))
import Fcf qualified
import Fcf.Data.Bool (UnBool)
import Obgs.Fcf.Branch (Guard, Otherwise, type (-->))
import Obgs.Fcf.Combinators (IterateN)
import Obgs.Fcf.Nat (NatsBetween)
import Obgs.TL.Error (ErrorMessage (..), TypeError, type (:$>:))
import Obgs.TL.Error qualified as TE
import Obgs.TL.Nat (Div, Nat, type (+), type (-), type (<?), type (>?))
import Obgs.TL.Type (type (==))
import Prelude
  ( Bool,
    Bounded (..),
    Enum (..),
    Eq (..),
    Maybe (..),
    Monad,
    Show,
    String,
    type (~),
    fromIntegral,
    ($),
    (.),
    (<$>),
  )

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
-- >>> import Data.Singletons.Base.Enum ()
--

-- | A type-level variant of 'Data.Enum.minBound'.
--
-- /Expects type __@a@__ to have a 'PBounded' instance/.
--
-- >>> :kind! Eval (MinBound @Bool)
-- ...
-- = False
data MinBound :: Exp a

type instance Eval (MinBound :: Exp a) = Sing.MinBound @a

-- | A type-level variant of 'Data.Enum.maxBound'.
--
-- /Expects type __@a@__ to have a 'PBounded' instance/.
--
-- >>> :kind! Eval (MaxBound @Bool)
-- ...
-- = True
data MaxBound :: Exp a

type instance Eval (MaxBound :: Exp a) = Sing.MaxBound @a

-- | A type-level variant of 'Data.Enum.succ'.
--
-- /Expects type __@a@__ to have a 'PEnum' instance/.
--
-- >>> :kind! Succ @@ 1
-- ...
-- = 2
data Succ :: a -> Exp a

type instance Eval (Succ a) = Sing.Succ a

-- | A type-level variant of 'Data.Enum.pred'.
--
-- /Expects type __@a@__ to have a 'PEnum' instance/.
--
-- >>> :kind! Pred @@ 1
-- ...
-- = 0
data Pred :: a -> Exp a

type instance Eval (Pred a) = Sing.Pred a

-- | A type-level variant of 'Data.Enum.fromEnum'.
--
-- /Expects type __@a@__ to have a 'PEnum' instance/.
--
-- >>> :kind! FromEnum @@ True
-- ...
-- = 1
data FromEnum :: a -> Exp Nat

type instance Eval (FromEnum a) = Sing.FromEnum a

-- | A type-level variant of 'Data.Enum.toEnum'.
--
-- /Expects type __@a@__ to have a 'PEnum' instance/.
--
-- >>> :kind! ToEnum @Bool @@ 1
-- ...
-- = True
data ToEnum :: Nat -> Exp a

type instance Eval (ToEnum n) = Sing.ToEnum n

-- | A type-level variant of 'Data.Enum.enumFromTo'.
--
-- /Expects type __@a@__ to have a 'PEnum' instance/.
--
-- >>> :kind! EnumFromTo 1 @@ 3
-- ...
-- = [1, 2, 3]
data EnumFromTo :: a -> a -> Exp [a]

type instance Eval (EnumFromTo a b) = Sing.EnumFromToSym1 a Sing.@@ b

-- | A type-level variant of 'Data.Enum.enumFromThenTo'.
--
-- /Expects type __@a@__ to have a 'PEnum' instance/.
--
-- >>> :kind! EnumFromThenTo 1 3 @@ 10
-- ...
-- = [1, 3, 5, 7, 9]
data EnumFromThenTo :: a -> a -> a -> Exp [a]

type instance Eval (EnumFromThenTo a b c) = Sing.EnumFromThenToSym2 a b Sing.@@ c

-- | Returns 'Just' the successor of a value, if any. Otherwise, returns 'Nothing'.
--
-- /Expects type __@a@__ to have 'PBounded' and 'PEnum' instances/.
--
-- >>> :kind! SuccMaybe @@ False
-- ...
-- = Just True
--
-- >>> :kind! SuccMaybe @@ True
-- ...
-- = Nothing
data SuccMaybe :: a -> Exp (Maybe a)

type instance
  Eval (SuccMaybe (x :: a)) =
    Eval (UnBool ('Just <$> Succ x) (Pure 'Nothing) (x == Sing.MaxBound @a))

-- | Returns 'Just' the predecessor of value, if any. Otherwise, returns 'Nothing'.
--
-- /Expects type __@a@__ to have 'PBounded' and 'PEnum' instances/.
--
-- >>> :kind! PredMaybe @@ True
-- ...
-- = Just False
--
-- >>> :kind! PredMaybe @@ False
-- ...
-- = Nothing
data PredMaybe :: a -> Exp (Maybe a)

type instance
  Eval (PredMaybe (x :: a)) =
    Eval (UnBool ('Just <$> Pred x) (Pure 'Nothing) (x == Sing.MinBound @a))

-- | Returns whether an enumerable type has multiple values.
--
-- /Expects type __@a@__ to have 'PBounded' and 'PEnum' instances/.
--
-- >>> :kind! IsMultipleEnum @@ Bool
-- ...
-- = True
--
-- >>> :kind! IsMultipleEnum @@ ()
-- ...
-- = False
data IsMultipleEnum :: Type -> Exp Bool

type instance
  Eval (IsMultipleEnum a) =
    Eval (Not =<< TyEq (Sing.MinBound @a) (Sing.MaxBound @a))

-- | A generic implementation of 'EnumFromTo', based on value indices.
--
-- Takes one optional parameter in addition to the usual ones:
-- a FCF usable to convert indices back to values of type __@a@__
-- (defaults to 'ToEnum').
--
-- /Expects type __@a@__ to have a 'PEnum' instance,/
-- /and value indices to be __0-based__ and __monotonically increasing__/.
data EnumFromToByIx :: Maybe (Nat -> Exp a) -> a -> a -> Exp [a]

type instance
  Eval (EnumFromToByIx toEnum from to) =
    Eval
      ( Fcf.Map (FromMaybe ToEnum @@ toEnum)
          =<< NatsBetween (Sing.FromEnum from) (Sing.FromEnum to)
      )

-- | Throws a 'TypeError' warning that a call to 'EnumFromThenTo' would result in an infinite list.
--
-- Takes four parameters:
--
--  * a FCF usable to show values of type __@a@__ in 'ErrorMessage's,
--  * the @from@ value,
--  * the @then@ value,
--  * the @to@ value.
data ThrowInfiniteEnumError :: (a -> Exp ErrorMessage) -> a -> a -> a -> Exp b

type instance
  Eval (ThrowInfiniteEnumError show from then_ to) =
    TypeError
      ( TE.Text "Prevented call to:"
          :$>: TE.Text "EnumFromThenTo "
          :<>: (show @@ from)
          :<>: TE.Text " "
          :<>: (show @@ then_)
          :<>: TE.Text " "
          :<>: (show @@ to)
          :$$: TE.Text "as it would result in an infinite list"
      )

-- | A generic implementation of 'EnumFromThenTo', based on value indices.
--
-- Takes two optional parameters in addition to the usual ones:
--
--  * a FCF usable to show values of type __@a@__ in 'ErrorMessage's (defaults to 'ShowType'),
--  * a FCF usable to convert indices back to values of type __@a@__ (defaults to 'ToEnum').
--
-- Throws a 'TypeError' if the result would be infinite.
--
-- /Expects type __@a@__ to have a 'PEnum' instance,/
-- /and value indices to be __0-based__ and __monotonically increasing__/.
data
  EnumFromThenToByIx ::
    Maybe (a -> Exp ErrorMessage) -> Maybe (Nat -> Exp a) -> a -> a -> a -> Exp [a]

type instance
  Eval (EnumFromThenToByIx show toEnum from then_ to) =
    Eval
      ( UnMaybe
          (ThrowInfiniteEnumError (FromMaybe (Fcf.Pure1 ShowType) @@ show) from then_ to)
          (Fcf.Map (FromMaybe ToEnum @@ toEnum))
          =<< NatEnumFromThenTo (Sing.FromEnum from) (Sing.FromEnum then_) (Sing.FromEnum to)
      )

-- | The underlying implementation of 'EnumFromThenToByCase', based on value indices.
data
  EnumFromThenToByCase' ::
    Exp [a] ->
    (Nat -> Nat -> Nat -> Exp [a]) ->
    (Nat -> Nat -> Nat -> Exp [a]) ->
    Nat ->
    Nat ->
    Nat ->
    Exp [a]

type instance
  Eval (EnumFromThenToByCase' throwInfiniteEnumError goAsc goDesc from then_ to) =
    Guard
      @@ '[ (from <? then_)
              --> Fcf.If
                    (from >? to)
                    (Fcf.Pure '[])
                    (goAsc from (then_ - from) ((to - from) `Div` (then_ - from) + 1)),
            (from >? then_)
              --> Fcf.If
                    (from <? to)
                    (Fcf.Pure '[])
                    (goDesc from (from - then_) ((from - to) `Div` (from - then_) + 1)),
            Otherwise
              --> Fcf.If
                    (from >? to)
                    (Fcf.Pure '[])
                    throwInfiniteEnumError
          ]

-- | A generic implementation of 'EnumFromThenTo' that takes care of handling the main logic,
-- and delegates the generation of the list to a user-defined FCF.
--
-- Takes three parameters in addition to the usual ones:
--
--  * a FCF usable to show values of type __@a@__ in 'ErrorMessage's (defaults to 'ShowType'),
--  * a FCF usable to generate lists of values in /ascending/ order,
--  * a FCF usable to generate lists of values in /descending/ order.
--
-- The FCFs responsible for generating the values should expect these three parameters:
--
--  * the index of the starting value,
--  * the number of values to skip between iterations,
--  * the total number of values to be generated.
--
-- Throws a 'TypeError' if the result would be infinite.
--
-- /Expects type __@a@__ to have a 'PEnum' instance,/
-- /and value indices to be __0-based__ and __monotonically increasing__/.
data
  EnumFromThenToByCase ::
    Maybe (a -> Exp ErrorMessage) ->
    (Nat -> Nat -> Nat -> Exp [a]) ->
    (Nat -> Nat -> Nat -> Exp [a]) ->
    a ->
    a ->
    a ->
    Exp [a]

type instance
  Eval (EnumFromThenToByCase show goAsc goDesc from then_ to) =
    Eval
      ( EnumFromThenToByCase'
          (ThrowInfiniteEnumError (FromMaybe (Fcf.Pure1 ShowType) @@ show) from then_ to)
          goAsc
          goDesc
          (Sing.FromEnum from)
          (Sing.FromEnum then_)
          (Sing.FromEnum to)
      )

-- | The underlying implementation of 'NatEnumFromThenTo', given precalculated step and size.
data
  NatEnumFromThenTo' ::
    (Nat -> Nat -> Exp Nat) ->
    Nat ->
    Nat ->
    Nat ->
    Exp [Nat]

type instance
  Eval (NatEnumFromThenTo' f from step size) =
    IterateN (f step) from @@ (size `Div` step)

-- | An empty result for 'NatEnumFromThenTo'.
type EmptyEnum = Fcf.Pure ('Just '[])

-- | A variant of of 'EnumFromThenTo' for 'Nat's.
--
-- Unlike the base instance from @singletons@,
-- this one has the same behavior as the term-level instance in all cases.
--
-- Returns 'Nothing' if the result would be infinite.
--
-- >>> :kind! Eval (NatEnumFromThenTo 1 3 10)
-- ...
-- = Just [1, 3, 5, 7, 9]
--
-- >>> :kind! Eval (NatEnumFromThenTo 10 7 1)
-- ...
-- = Just [10, 7, 4, 1]
--
-- >>> :kind! Eval (NatEnumFromThenTo 1 1 10)
-- ...
-- = Nothing
data NatEnumFromThenTo :: Nat -> Nat -> Nat -> Exp (Maybe [Nat])

type instance
  Eval (NatEnumFromThenTo from then_ to) =
    Guard
      @@ '[ (from <? then_)
              --> Fcf.If
                    (from >? to)
                    EmptyEnum
                    ( 'Just
                        Fcf.<$> NatEnumFromThenTo'
                                  (Fcf.+)
                                  from
                                  (then_ - from)
                                  (to - from)
                    ),
            (from >? then_)
              --> Fcf.If
                    (from <? to)
                    EmptyEnum
                    ( 'Just
                        Fcf.<$> NatEnumFromThenTo'
                                  (Fcf.Flip (Fcf.-))
                                  from
                                  (from - then_)
                                  (from - to)
                    ),
            Otherwise
              --> Fcf.If
                    (from >? to)
                    EmptyEnum
                    (Fcf.Pure 'Nothing)
          ]

-- | Runs a suite of tests to verify that the 'PBounded' and 'PEnum' instances
-- of a type behave like the corresponding term-level 'Bounded' and 'Enum'
-- instances.
--
-- This function should only be used to test small types, as it enumerates
-- every value between 'minBound' and 'maxBound'.
testBoundedEnumInstance ::
  forall
    a
    spec
    m
    minBound'
    maxBound'
    xs
    allSucc
    allPred
    ixes
    roundTripped.
  ( Monad m,
    Monad spec,
    Eq a,
    Show a,
    SingKind a,
    Demote a ~ a,
    Bounded a,
    Enum a,
    PBounded a,
    PEnum a,
    minBound' ~ Eval (MinBound :: Exp a),
    maxBound' ~ Eval (MaxBound :: Exp a),
    xs ~ Eval (EnumFromTo minBound' maxBound'),
    allSucc ~ Eval (Map SuccMaybe xs),
    allPred ~ Eval (Map PredMaybe xs),
    ixes ~ Eval (Map FromEnum xs),
    roundTripped ~ Eval (Map (ToEnum :: Nat -> Exp a) ixes),
    SingI minBound',
    SingI maxBound',
    SingI xs,
    SingI allSucc,
    SingI allPred,
    SingI ixes,
    SingI roundTripped
  ) =>
  (String -> spec () -> m ()) ->
  (forall b. (Eq b, Show b) => b -> b -> spec ()) ->
  Proxy a ->
  m ()
testBoundedEnumInstance specify shouldBe _ = do
  let values = [minBound .. maxBound] :: [a]
  let succMaybe x = if x == maxBound then Nothing else Just (succ x)
  let predMaybe x = if x == minBound then Nothing else Just (pred x)

  specify "MinBound" $
    demote @minBound' `shouldBe` minBound
  specify "MaxBound" $
    demote @maxBound' `shouldBe` maxBound
  specify "EnumFromTo" $
    demote @xs `shouldBe` values
  specify "SuccMaybe" $
    demote @allSucc `shouldBe` (succMaybe <$> values)
  specify "PredMaybe" $
    demote @allPred `shouldBe` (predMaybe <$> values)
  specify "FromEnum" $
    demote @ixes `shouldBe` (fromIntegral . fromEnum <$> values)
  specify "'FromEnum' and 'ToEnum' have an inverse relationship" $
    demote @roundTripped `shouldBe` values

-- | Runs a suite of tests to verify that a 'PEnum' instance behaves like the
-- corresponding term-level 'Enum' instance.
--
-- This function is appropriate for types without a 'Bounded' instance, but
-- only tests the properties reachable from the three given values. The first
-- of them must have both a successor and a predecessor.
testEnumInstance ::
  forall
    a
    (from :: a)
    (then_ :: a)
    (to :: a)
    spec
    m
    succFrom
    predFrom
    ixFrom
    roundTripped
    fromTo
    fromThenTo.
  ( Monad m,
    Monad spec,
    Eq a,
    Show a,
    SingKind a,
    Demote a ~ a,
    Enum a,
    PEnum a,
    succFrom ~ Eval (Succ from),
    predFrom ~ Eval (Pred from),
    ixFrom ~ Eval (FromEnum from),
    roundTripped ~ Eval (ToEnum ixFrom :: Exp a),
    fromTo ~ Eval (EnumFromTo from to),
    fromThenTo ~ Eval (EnumFromThenTo from then_ to),
    SingI from,
    SingI then_,
    SingI to,
    SingI succFrom,
    SingI predFrom,
    SingI ixFrom,
    SingI roundTripped,
    SingI fromTo,
    SingI fromThenTo
  ) =>
  (String -> spec () -> m ()) ->
  (forall b. (Eq b, Show b) => b -> b -> spec ()) ->
  Proxy '(from, then_, to) ->
  m ()
testEnumInstance specify shouldBe _ = do
  let from = demote @from
  let then' = demote @then_
  let to = demote @to

  specify "Succ" $
    demote @succFrom `shouldBe` succ from
  specify "Pred" $
    demote @predFrom `shouldBe` pred from
  specify "FromEnum" $
    demote @ixFrom `shouldBe` fromIntegral (fromEnum from)
  specify "'FromEnum' and 'ToEnum' have an inverse relationship" $
    demote @roundTripped `shouldBe` from
  specify "EnumFromTo" $
    demote @fromTo `shouldBe` enumFromTo from to
  specify "EnumFromThenTo" $
    demote @fromThenTo `shouldBe` enumFromThenTo from then' to
