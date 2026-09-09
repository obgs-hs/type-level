{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module      : $Header$
-- Description : First-class families for working with 'Finitary' types.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for working with 'Finitary' types.
--
-- /All families in this module expect the types involved to have a 'PFinitary' instance/.
module Obgs.Fcf.Finitary
  ( -- * Type-level variants
    Cardinality,
    Start,
    End,
    Previous,
    Next,
    FromFinite,
    ToFinite,
    Inhabitants,
    InhabitantsFrom,
    InhabitantsTo,
    InhabitantsFromTo,

    -- * Indices
    Index,
    ToIndex,
    ToIndexMaybe,
    FromIndex,

    -- * Testing
    testPFinitaryInstance,
    testPFinitarySmallInstance,
  )
where

import Data.Finitary hiding (Cardinality)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Ord.Singletons (Max, Min)
import Data.Proxy (Proxy)
import Data.Singletons (SingI, SingKind (..), demote)
import Debug.Trace
import Fcf (Eval, Exp, Flip, Map, type (<=<), type (=<<), type (@@))
import Obgs.Fcf.Enum
import Obgs.TL.Finitary (Index, PFinitary)
import Obgs.TL.Finitary qualified as TF
import Obgs.TL.List qualified as TL
import Obgs.TL.Maybe (FromMaybe, type (??))
import Obgs.TL.Nat (Div, Mod, Nat, SubtractMaybe, type (-), type (<=))
import Prelude

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
-- >>> import Data.Finitary ()
--

-- | A lifted variant of 'Data.Finitary.Cardinality'.
--
-- >>> :kind! Cardinality @@ Bool
-- ...
-- = 2
data Cardinality (a :: Type) :: Exp Nat

type instance Eval (Cardinality a) = TF.Cardinality a

-- | Converts a 'Nat' to the corresponding 'Index'.
--
-- Throws a 'Obgs.TL.Error.TypeError' if the 'Nat' is out of bounds.
data ToIndex :: Nat -> Exp (Index a)

type instance Eval (ToIndex n) = TF.ToIndex n

-- | Converts a 'Nat' to the corresponding 'Index'.
--
-- Returns 'Nothing' if the 'Nat' is out of bounds.
data ToIndexMaybe :: Nat -> Exp (Maybe (Index a))

type instance Eval (ToIndexMaybe n) = TF.ToIndexMaybe n

-- | Converts an 'Index' to the corresponding 'Nat'.
data FromIndex :: Index a -> Exp Nat

type instance Eval (FromIndex ix) = TF.FromIndex ix

-- | A type-level variant of 'Data.Finitary.start'.
--
-- >>> :kind! Eval (Start @Bool)
-- ...
-- = False
data Start :: Exp a

type instance Eval Start = TF.Start

-- | A type-level variant of 'Data.Finitary.end'.
--
-- >>> :kind! Eval (End @Bool)
-- ...
-- = True
data End :: Exp a

type instance Eval End = TF.End

-- | A type-level variant of 'Data.Finitary.previous'.
--
-- >>> :kind! Previous @@ 'True
-- ...
-- = Just False
--
-- >>> :kind! Previous @@ 'False
-- ...
-- = Nothing
data Previous :: a -> Exp (Maybe a)

type instance Eval (Previous x) = TF.Previous x

-- | A type-level variant of 'Data.Finitary.next'.
--
-- >>> :kind! Next @@ 'False
-- ...
-- = Just True
--
-- >>> :kind! Next @@ 'True
-- ...
-- = Nothing
data Next :: a -> Exp (Maybe a)

type instance Eval (Next x) = TF.Next x

-- | A type-level variant of 'Data.Finitary.fromIndex'.
--
-- >>> :kind! Eval (FromFinite =<< ToIndex @Bool 1)
-- ...
-- = True
data FromFinite :: Index a -> Exp a

type instance Eval (FromFinite n) = Eval (ToEnum =<< FromIndex n)

-- | A type-level variant of 'Data.Finitary.toIndex'.
--
-- >>> :kind! Eval (FromIndex =<< ToFinite @Bool 'True)
-- ...
-- = 1
data ToFinite :: a -> Exp (Index a)

type instance Eval (ToFinite x) = Eval (ToIndex =<< FromEnum x)

-- | A type-level variant of 'Data.Finitary.inhabitants'.
--
-- >>> :kind! Eval (Inhabitants @Bool)
-- ...
-- = [False, True]
data Inhabitants :: Exp [a]

type instance Eval (Inhabitants @a) = TF.Inhabitants @a

-- | A type-level variant of 'Data.Finitary.inhabitantsFrom'.
--
-- >>> :kind! InhabitantsFrom @@ 'True
-- ...
-- = '[True]
data InhabitantsFrom :: a -> Exp [a]

type instance Eval (InhabitantsFrom from) = TF.InhabitantsFrom from

-- | A type-level variant of 'Data.Finitary.inhabitantsTo'.
--
-- >>> :kind! InhabitantsTo @@ 'False
-- ...
-- = '[False]
data InhabitantsTo :: a -> Exp [a]

type instance Eval (InhabitantsTo to) = TF.InhabitantsTo to

-- | A type-level variant of 'Data.Finitary.inhabitantsFromTo'.
--
-- >>> :kind! Eval (InhabitantsFromTo 'a' 'e')
-- ...
-- = ['a', 'b', 'c', 'd', 'e']
data InhabitantsFromTo :: a -> a -> Exp [a]

type instance Eval (InhabitantsFromTo from to) = TF.InhabitantsFromTo from to

-- | Runs a suite of tests to verify that a 'PFinitary' instance
-- behaves like the corresponding term-level 'Finitary' instance.
--
-- This function should only be used to test small 'PFinitary' instances,
-- as it enumerates all 'inhabitants' of the tested type.
testPFinitarySmallInstance ::
  forall
    a
    spec
    m
    start
    end
    xs
    ixes
    fromIxes
    toIxes
    allPrevious
    allNext
    fromStart
    toEnd
    from
    to
    fromTo
    fromToReversed.
  ( Monad m,
    Monad spec,
    Eq a,
    Show a,
    SingKind a,
    Demote a ~ a,
    Finitary a,
    PFinitary a,
    1 <= TF.Cardinality a,
    start ~ TF.Start @a,
    end ~ TF.End @a,
    xs ~ TF.Inhabitants @a,
    ixes ~ TF.Indices a,
    allPrevious ~ Map Previous @@ xs,
    allNext ~ Map Next @@ xs,
    fromIxes ~ Map (FromFinite @a) @@ ixes,
    toIxes ~ Map (FromIndex <=< ToFinite @a) @@ xs,
    fromStart ~ FromMaybe start (TF.Next start),
    toEnd ~ FromMaybe end (TF.Previous end),
    from ~ TF.InhabitantsFrom @a fromStart,
    to ~ TF.InhabitantsTo @a toEnd,
    fromTo ~ TF.InhabitantsFromTo @a fromStart toEnd,
    fromToReversed ~ TF.InhabitantsFromTo @a toEnd fromStart,
    SingI start,
    SingI end,
    SingI xs,
    SingI allPrevious,
    SingI allNext,
    SingI fromIxes,
    SingI toIxes,
    SingI fromStart,
    SingI toEnd,
    SingI from,
    SingI to,
    SingI fromTo,
    SingI fromToReversed
  ) =>
  (String -> spec () -> m ()) ->
  (forall b. (Eq b, Show b) => b -> b -> spec ()) ->
  Proxy a ->
  m ()
testPFinitarySmallInstance specify shouldBe _ = do
  specify "Start" $
    demote @start `shouldBe` start
  specify "End" $
    demote @end `shouldBe` end
  specify "Previous" $
    demote @allPrevious `shouldBe` (previous <$> inhabitants @a)
  specify "Next" $
    demote @allNext `shouldBe` (next <$> inhabitants @a)
  specify "FromFinite" $
    demote @fromIxes `shouldBe` inhabitants
  specify "ToFinite" $
    demote @toIxes `shouldBe` (fromIntegral . toFinite <$> inhabitants @a)
  specify "Inhabitants" $
    demote @xs `shouldBe` inhabitants

  let fromStart = demote @fromStart
  let toEnd = demote @toEnd

  specify "InhabitantsFrom" $
    demote @from `shouldBe` toList (inhabitantsFrom fromStart)
  specify "InhabitantsTo" $
    demote @to `shouldBe` toList (inhabitantsTo toEnd)
  specify "InhabitantsFromTo" $ do
    demote @fromTo `shouldBe` toList (inhabitantsFromTo fromStart toEnd)
    demote @fromToReversed `shouldBe` toList (inhabitantsFromTo toEnd fromStart)

-- | Runs a suite of tests to verify that a 'PFinitary' instance
-- behaves like the corresponding term-level 'Finitary' instance.
--
-- This function is appropriate for testing large 'PFinitary' instances,
-- but only tests a subset of the properties tested by 'testPFinitarySmallInstance'.
testPFinitaryInstance ::
  forall
    a
    spec
    m
    count
    start
    end
    lastStartAt
    firstEndAt
    lastStartIx
    firstEndIx
    startIxes
    endIxes
    lastStart
    firstEnd
    from
    to
    startPrevious
    endPrevious
    startNext
    endNext
    fromStartIxes
    fromEndIxes
    toStartIxes
    toEndIxes
    fromToStart
    fromToEnd.
  ( Monad m,
    Monad spec,
    Eq a,
    Show a,
    SingKind a,
    Demote a ~ a,
    Finitary a,
    PFinitary a,
    count ~ TF.Cardinality a,
    1 <= count,
    start ~ TF.Start @a,
    end ~ TF.End @a,
    lastStartAt ~ Min 2 (count `Div` 2),
    firstEndAt ~ (count - lastStartAt) `Mod` count,
    lastStartIx ~ TF.ToIndex lastStartAt,
    firstEndIx ~ TF.ToIndex firstEndAt,
    startIxes ~ Eval (Flip EnumFromTo lastStartIx =<< MinBound),
    endIxes ~ Eval (EnumFromTo firstEndIx =<< MaxBound),
    lastStart ~ TF.FromFinite lastStartIx,
    firstEnd ~ TF.FromFinite firstEndIx,
    from ~ TF.InhabitantsFrom firstEnd,
    to ~ TF.InhabitantsTo lastStart,
    startPrevious ~ Map Previous @@ from,
    endPrevious ~ Map Previous @@ to,
    startNext ~ Map Next @@ from,
    endNext ~ Map Next @@ to,
    fromStartIxes ~ Map (FromFinite @a) @@ startIxes,
    fromEndIxes ~ Map (FromFinite @a) @@ endIxes,
    toStartIxes ~ Map (FromIndex <=< ToFinite @a) @@ fromStartIxes,
    toEndIxes ~ Map (FromIndex <=< ToFinite @a) @@ fromEndIxes,
    fromToStart ~ TF.InhabitantsFromTo start lastStart,
    fromToEnd ~ TF.InhabitantsFromTo firstEnd end,
    SingI count,
    SingI start,
    SingI end,
    SingI lastStartAt,
    SingI firstEndAt,
    SingI from,
    SingI to,
    SingI startPrevious,
    SingI endPrevious,
    SingI startNext,
    SingI endNext,
    SingI fromStartIxes,
    SingI fromEndIxes,
    SingI toStartIxes,
    SingI toEndIxes,
    SingI fromToStart,
    SingI fromToEnd
  ) =>
  (String -> spec () -> m ()) ->
  (forall b. (Eq b, Show b) => b -> b -> spec ()) ->
  Proxy a ->
  m ()
testPFinitaryInstance specify shouldBe _ = do
  specify "Start" $
    demote @start `shouldBe` start
  specify "End" $
    demote @end `shouldBe` end

  let lastStartIx = fromIntegral $ demote @lastStartAt
  let firstEndIx = fromIntegral $ demote @firstEndAt
  let startIxes = inhabitantsTo lastStartIx
  let endIxes = inhabitantsFrom firstEndIx
  let lastStart = fromFinite lastStartIx
  let firstEnd = fromFinite firstEndIx
  let from = toList $ inhabitantsFrom firstEnd
  let to = toList $ inhabitantsTo lastStart

  specify "Previous" $ do
    demote @startPrevious `shouldBe` (previous <$> from)
    demote @endPrevious `shouldBe` (previous <$> to)
  specify "Next" $ do
    demote @startNext `shouldBe` (next <$> from)
    demote @endNext `shouldBe` (next <$> to)
  specify "FromFinite" $ do
    demote @fromStartIxes `shouldBe` toList (fromFinite <$> startIxes)
    demote @fromEndIxes `shouldBe` toList (fromFinite <$> endIxes)
  specify "ToFinite" $ do
    demote @toStartIxes `shouldBe` toList (fromIntegral <$> startIxes)
    demote @toEndIxes `shouldBe` toList (fromIntegral <$> endIxes)
  specify "InhabitantsFrom" $
    demote @from `shouldBe` toList (inhabitantsFrom firstEnd)
  specify "InhabitantsTo" $
    demote @to `shouldBe` toList (inhabitantsTo lastStart)
  specify "InhabitantsFromTo" $ do
    demote @fromToStart `shouldBe` toList (inhabitantsFromTo start lastStart)
    demote @fromToEnd `shouldBe` toList (inhabitantsFromTo firstEnd end)
