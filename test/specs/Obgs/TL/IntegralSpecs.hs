{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Obgs.TL.IntegralSpecs where

import Control.Exception (SomeException, catch, evaluate)
import Data.Bifunctor (bimap)
import Data.Constraint (Dict (..))
import Data.Finitary
import Data.Finite (Finite, finite)
import Data.Foldable (toList)
import Data.Int
import Data.Singletons (SingI, SingKind, demote)
import Data.Singletons.Base.Enum (PBounded (..))
import Data.Type.Ord (type (<=))
import Data.Typeable
import Data.Word (Word16, Word32, Word64, Word8)
import Fcf (Eval, type (<=<), type (@@))
import Fcf qualified hiding (type (*), type (+), type (-), type (<), type (<=), type (>), type (>=))
import GHC.TypeLits (KnownNat, natVal)
import Obgs.Fcf.Enum qualified as Fcf
import Obgs.Fcf.Finitary qualified as Fcf
import Obgs.Fcf.List qualified as Fcf
import Obgs.Fcf.Maybe qualified as Fcf
import Obgs.Fcf.Num qualified as Fcf
import Obgs.Fcf.Ord qualified as Fcf
import Obgs.Fcf.Tuple qualified as Fcf
import Obgs.TL.Finitary (End, PFinitary, Start)
import Obgs.TL.Integral hiding (Finite)
import Prelude.Singletons qualified as Sing
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, forAll, suchThat)
import Test.QuickCheck qualified as QC

typeLevelIntegralSpecs :: SpecWith ()
typeLevelIntegralSpecs = do
  integralTypeSpecs @Integer
  integralTypeSpecs @Int
  integralTypeSpecs @Int8
  integralTypeSpecs @Int16
  integralTypeSpecs @Int32
  integralTypeSpecs @Int64
  integralTypeSpecs @Word
  integralTypeSpecs @Word8
  integralTypeSpecs @Word16
  integralTypeSpecs @Word32
  integralTypeSpecs @Word64
  integralTypeSpecs @(Finite 128)

  describe "Inhabitants" $ do
    specify "returns all inhabitants" $ do
      demote @(Inhabitants Int8) `shouldBe` inhabitants
      demote @(Inhabitants (Finite 32)) `shouldBe` inhabitants

integralTypeSpecs :: forall a. (TestableIntegral a) => SpecWith ()
integralTypeSpecs = do
  let integralType = show $ typeRep (Proxy @a)

  describe ("TypeIntegral " <> integralType) $ do
    prop "is an isomorphism" $ \(n :: a) ->
      toIntegral (toTypeIntegral n) `shouldBe` n

    describe "preserves the 'Ord' instance" $ do
      specify "compare" $ do
        forAll (arbitrary @(a, a)) $
          testBinaryOp @Ord compare
      specify "<" $ do
        forAll (arbitrary @(a, a)) $
          testBinaryOp @Ord (<)
      specify "<=" $ do
        forAll (arbitrary @(a, a)) $
          testBinaryOp @Ord (<=)
      specify ">" $ do
        forAll (arbitrary @(a, a)) $
          testBinaryOp @Ord (>)
      specify ">=" $ do
        forAll (arbitrary @(a, a)) $
          testBinaryOp @Ord (>=)

    describe "has a valid 'POrd' instance" $ do
      let args = demote @(ReferenceCompareArgs a)

      specify "Compare" $ do
        demote @(ReferenceCompareResults a Fcf.Compare) `shouldBe` (uncurry compare <$> args)
      specify "<" $ do
        demote @(ReferenceCompareResults a (Fcf.<)) `shouldBe` (uncurry (<) <$> args)
      specify "<=" $ do
        demote @(ReferenceCompareResults a (Fcf.<=)) `shouldBe` (uncurry (<=) <$> args)
      specify ">" $ do
        demote @(ReferenceCompareResults a (Fcf.>)) `shouldBe` (uncurry (>) <$> args)
      specify ">=" $ do
        demote @(ReferenceCompareResults a (Fcf.>=)) `shouldBe` (uncurry (>=) <$> args)
      specify "Min" $ do
        demote @(ReferenceCompareResults a Fcf.Min) `shouldBe` (uncurry min <$> args)
      specify "Max" $ do
        demote @(ReferenceCompareResults a Fcf.Max) `shouldBe` (uncurry max <$> args)

    describe "preserves the 'Num' instance" $ do
      specify "+" $ do
        forAll (arbitrary @(a, a)) $
          testIsomorphism2 @Num (+)
      specify "-" $ do
        forAll (arbitrary @(a, a)) $
          testIsomorphism2 @Num (-)
      specify "*" $ do
        forAll (arbitrary @(a, a)) $
          testIsomorphism2 @Num (*)
      specify "negate" $ do
        forAll (arbitrary @a) $
          testIsomorphism @Num negate
      specify "fromInteger" $ do
        forAll (arbitrary @Integer) $ \n ->
          toIntegral @a (fromInteger n) `shouldBeOrThrowLike` fromInteger n
      specify "sequence of operations" $ do
        forAll (arbitrary @([NumOp a], a)) $
          uncurry (testIsomorphismN @Num applyNumOp)

    describe "has a valid 'PNum' instance" $ do
      let unaryOpArgs = demote @(ReferenceNumArgs a)
      let binaryOpArgs = demote @(ReferenceNumBinaryOpArgs a)

      specify "+" $ do
        demote @(ReferenceNumBinaryOpResults a (Fcf.+)) `shouldBe` (uncurry (+) <$> binaryOpArgs)
      specify "-" $ do
        let subtractArgs = demote @(ReferenceSubtractArgs a)
        demote @(ReferenceSubtractResults a) `shouldBe` (uncurry (-) <$> subtractArgs)
      specify "*" $ do
        demote @(ReferenceNumBinaryOpResults a (Fcf.*)) `shouldBe` (uncurry (*) <$> binaryOpArgs)
      specify "Abs" $ do
        demote @(ReferenceNumUnaryOpResults a Fcf.Abs) `shouldBe` (abs <$> unaryOpArgs)
      specify "Signum" $ do
        demote @(ReferenceNumUnaryOpResults a Fcf.Signum) `shouldBe` (signum <$> unaryOpArgs)
      specify "FromInteger" $ do
        let args = fromIntegral <$> demote @ReferenceFromIntegerArgs
        demote @(ReferenceFromIntegerResults a) `shouldBe` (fromInteger <$> args)

      case demote @(ReferenceNegateArgs a) of
        [] -> pure ()
        args -> specify "Negate" $ demote @(ReferenceNegateResults a) `shouldBe` (negate <$> args)

    describe "preserves the 'Integral' instance" $ do
      specify "quot" $ do
        forAll (divArgsGen @a) $
          testIsomorphism2 @Integral quot
      specify "rem" $ do
        forAll (divArgsGen @a) $
          testIsomorphism2 @Integral rem
      specify "div" $ do
        forAll (divArgsGen @a) $
          testIsomorphism2 @Integral div
      specify "mod" $ do
        forAll (divArgsGen @a) $
          testIsomorphism2 @Integral mod
      specify "quotRem" $ do
        forAll (divArgsGen @a) $
          testIsomorphismPair @Integral quotRem
      specify "divMod" $ do
        forAll (divArgsGen @a) $
          testIsomorphismPair @Integral divMod
      specify "sequence of operations" $ do
        forAll (arbitrary @([IntegralOp a], a)) $
          uncurry (testIsomorphismN @Integral applyIntegralOp)

    withJust (boundedDict @a) $ \Dict -> do
      describe "preserves the 'Bounded' instance" $ do
        specify "minBound" $
          testIsomorphism0 @Bounded @a minBound
        specify "maxBound" $
          testIsomorphism0 @Bounded @a maxBound

      describe "has a valid 'PBounded' instance" $ do
        specify "MinBound" $ do
          toIntegral (demote @(MinBound :: TypeIntegral a)) `shouldBe` minBound
        specify "MaxBound" $ do
          toIntegral (demote @(MaxBound :: TypeIntegral a)) `shouldBe` maxBound

    describe "preserves the 'Enum' instance" $ do
      specify "succ" $ do
        forAll (arbitrary @a `suchThat` hasSucc) $
          testIsomorphism @Enum succ
      specify "pred" $ do
        forAll (arbitrary @a `suchThat` hasPred) $
          testIsomorphism @Enum pred
      specify "toEnum" $ do
        forAll (arbitrary @Int `suchThat` withinBoundsOf @a) $ \ix ->
          toIntegral (toEnum ix) `shouldBe` toEnum @a ix
      specify "fromEnum" $ do
        forAll (arbitrary @a `suchThat` withinBoundsOf @Int) $ \n ->
          fromEnum n `shouldBe` fromEnum (toTypeIntegral n)
      specify "enumFrom" $ do
        forAll (arbitrary @a) $ \n ->
          take 100 (toIntegral <$> enumFrom (toTypeIntegral n))
            `shouldBe` take 100 (enumFrom n)
      specify "enumFromTo" $ do
        forAll (arbitrary @(a, a)) $ \(m, n) ->
          take 100 (toIntegral <$> enumFromTo (toTypeIntegral m) (toTypeIntegral n))
            `shouldBe` take 100 (enumFromTo m n)
      specify "enumFromThenTo" $ do
        forAll (arbitrary @(a, a, a)) $ \(m, n, o) ->
          take 100 (toIntegral <$> enumFromThenTo (toTypeIntegral m) (toTypeIntegral n) (toTypeIntegral o))
            `shouldBe` take 100 (enumFromThenTo m n o)

    describe "has a valid 'PEnum' instance" $ do
      withJust (boundedDict @a) $ \Dict -> do
        specify "'FromEnum' and 'ToEnum' have an inverse relationship" $ do
          let args = demote @(ReferenceToFromEnumArgs a)
          demote @(ReferenceToFromEnumResults a) `shouldBe` args
      specify "EnumFromTo" $ do
        let args = demote @(ReferenceEnumFromToArgs a)
        demote @(ReferenceEnumFromToResults a) `shouldBe` (uncurry enumFromTo <$> args)
      specify "EnumFromThenTo" $ do
        let args = demote @(ReferenceEnumFromThenToArgs a)
        demote @(ReferenceEnumFromThenToResults a) `shouldBe` (uncurry3 enumFromThenTo <$> args)

    withJust (finitaryDict @a) $ \Dict -> do
      describe "preserves the 'Finitary' instance" $ do
        specify "start" $ do
          testIsomorphism0 @(TestableFinitary a) @a start
        specify "end" $ do
          testIsomorphism0 @(TestableFinitary a) @a end
        specify "fromFinite" $
          forAll (arbitrary @(Finite (Cardinality a))) $ \n ->
            toIntegral (fromFinite @(TypeIntegral a) n) `shouldBe` fromFinite @a n
        specify "toFinite" $
          forAll (arbitrary @a) $
            testUnaryOp @(TestableFinitary a) @a @(Finite (Cardinality a)) toFinite
        specify "previous" $
          forAll (arbitrary @a) $
            testIsomorphismF @(TestableFinitary a) @Maybe previous
        specify "next" $
          forAll (arbitrary @a) $
            testIsomorphismF @(TestableFinitary a) @Maybe next

      describe "has a valid 'PFinitary' instance" $ do
        let previousNextArgs = demote @(ReferencePreviousNextArgs a)

        specify "Start" $ do
          toIntegral (demote @(Start :: TypeIntegral a)) `shouldBe` start
        specify "End" $ do
          toIntegral (demote @(End :: TypeIntegral a)) `shouldBe` end
        specify "Next" $ do
          demote @(ReferenceNextResults a) `shouldBe` (next <$> previousNextArgs)
        specify "Previous" $ do
          demote @(ReferencePreviousResults a) `shouldBe` (previous <$> previousNextArgs)
        specify "InhabitantsFrom" $ do
          let args = demote @(ReferenceInhabitantsFromArg a)
          demote @(ReferenceInhabitantsFromResults a) `shouldBe` toList (inhabitantsFrom args)
        specify "InhabitantsTo" $ do
          let args = demote @(ReferenceInhabitantsToArg a)
          demote @(ReferenceInhabitantsToResults a) `shouldBe` toList (inhabitantsTo args)
        specify "InhabitantsFromTo" $ do
          let args = demote @(ReferenceEnumFromToArgs a)
          demote @(ReferenceInhabitantsFromToResults a) `shouldBe` (uncurry inhabitantsFromTo <$> args)

    withJust (boundedDict @a) $ \Dict -> do
      describe "overflow/underflow behavior matches term-level" $ do
          specify "addition overflow" $
            testIsomorphism2 @Num (+) (maxBound @a, 1)
          specify "subtraction underflow" $
            testIsomorphism2 @Num (-) (minBound @a, 1)

-- | The 'Constraint' for 'Integral' types that have testable 'TypeIntegral' counterparts.
type IsTestableIntegral a =
  ( Arbitrary a,
    Enum a,
    Show a,
    ToTypeIntegral a,
    Typeable a,
    Enum (TypeIntegral a),
    Show (TypeIntegral a),
    SingKind (TypeIntegral a),
    SingI (ReferenceFrom a),
    SingI (ReferenceThen a),
    SingI (ReferenceTo a),
    SingI (ReferenceCompareArgs a),
    SingI (ReferenceCompareResults a Fcf.Compare),
    SingI (ReferenceCompareResults a (Fcf.<)),
    SingI (ReferenceCompareResults a (Fcf.<=)),
    SingI (ReferenceCompareResults a (Fcf.>)),
    SingI (ReferenceCompareResults a (Fcf.>=)),
    SingI (ReferenceCompareResults a Fcf.Min),
    SingI (ReferenceCompareResults a Fcf.Max),
    SingI (ReferenceNumArgs a),
    SingI (ReferenceNumBinaryOpArgs a),
    SingI (ReferenceNumBinaryOpResults a (Fcf.+)),
    SingI (ReferenceNumBinaryOpResults a (Fcf.*)),
    SingI (ReferenceSubtractArgs a),
    SingI (ReferenceSubtractResults a),
    SingI (ReferenceNumUnaryOpResults a Fcf.Abs),
    SingI (ReferenceNumUnaryOpResults a Fcf.Signum),
    SingI (ReferenceNegateArgs a),
    SingI (ReferenceNegateResults a),
    SingI (ReferenceFromIntegerResults a),
    SingI (ReferenceEnumFromToArgs a),
    SingI (ReferenceEnumFromToResults a),
    SingI (ReferenceEnumFromThenToArgs a),
    SingI (ReferenceEnumFromThenToResults a)
  )

-- | Some predefined sets of arguments usable for testing binary 'POrd' operations.
type ReferenceCompareArgs a =
  '[ '(ReferenceFrom a, ReferenceTo a),
     '(ReferenceTo a, ReferenceFrom a),
     '(ReferenceFrom a, ReferenceFrom a)
   ]

-- | The results of testing some binary 'POrd' operation against the predefined sets of arguments.
type ReferenceCompareResults a op = Fcf.Map (Fcf.Uncurry op) @@ ReferenceCompareArgs a

-- | Some predefined arguments usable for testing 'Num' operations.
type ReferenceNumArgs a =
  Fcf.CatMaybes
    @@ '[ MinusMaybe 10,
          MinusMaybe 3,
          'Just Zero,
          PlusMaybe 3,
          PlusMaybe 10
        ] ::
    [TypeIntegral a]

-- | The results of testing some unary 'Num' operation against the predefined arguments.
type ReferenceNumUnaryOpResults a op = Fcf.Map op @@ ReferenceNumArgs a

-- | Some predefined sets of arguments usable for testing binary 'Num' operations.
type ReferenceNumBinaryOpArgs a = Fcf.CartesianProduct (ReferenceNumArgs a) @@ ReferenceNumArgs a

-- The results of testing some binary 'Num' operation against the predefined sets of arguments.
type ReferenceNumBinaryOpResults a op = Fcf.Map (Fcf.Uncurry op) @@ ReferenceNumBinaryOpArgs a

-- | Some predefined sets of arguments usable for testing 'Subtract' for types that
-- do not support negative numbers.
type ReferenceSubtractPositiveArgs a = Fcf.Filter (Fcf.Uncurry (Fcf.>)) @@ ReferenceNumBinaryOpArgs a

-- | Some predefined sets of arguments usable for testing 'Subtract'.
type ReferenceSubtractArgs a =
  Fcf.UnMaybe
    (Fcf.Pure (ReferenceSubtractPositiveArgs a))
    (Fcf.ConstFn (ReferenceNumBinaryOpArgs a))
    @@ MinusBound a

-- | The results of testing 'Subtract' against the predefined sets of arguments.
type ReferenceSubtractResults a = Fcf.Map (Fcf.Uncurry (Fcf.-)) @@ ReferenceSubtractArgs a

-- | Some predefined arguments usable for testing 'Negate'.
type ReferenceNegateArgs a =
  Fcf.UnMaybe
    (Fcf.Pure '[])
    (Fcf.ConstFn (ReferenceNumArgs a))
    @@ MinusBound a

-- | The results of testing 'Negate' against the predefined arguments.
type ReferenceNegateResults a = Fcf.Map Fcf.Negate @@ ReferenceNegateArgs a

-- | Some predefined arguments usable for testing 'FromInteger'.
type ReferenceFromIntegerArgs = '[0, 3, 10]

-- | The results of testing 'FromInteger' against the predefined arguments.
type ReferenceFromIntegerResults a =
  Fcf.Map Fcf.FromInteger @@ ReferenceFromIntegerArgs :: [TypeIntegral a]

-- | Some predefined arguments usable for testing 'FromEnum' + 'ToEnum'.
type ReferenceToFromEnumArgs a =
  '[ReferenceFrom a, ReferenceThen a, ReferenceTo a]

-- | The results of testing 'FromEnum' + 'ToEnum' against the predefined sets of arguments.
type ReferenceToFromEnumResults a =
  Fcf.Map (Fcf.ToEnum @(TypeIntegral a) <=< Fcf.FromEnum) @@ ReferenceToFromEnumArgs a

-- | Some predefined sets of arguments usable for testing 'EnumFromTo'.
type ReferenceEnumFromToArgs a =
  Fcf.Permutations @@ '(ReferenceFrom a, ReferenceTo a)

-- | The results of testing 'EnumFromTo' against the predefined sets of arguments.
type ReferenceEnumFromToResults a =
  Fcf.Map (Fcf.Uncurry Fcf.EnumFromTo) @@ ReferenceEnumFromToArgs a

-- | Some predefined sets of arguments usable for testing 'EnumFromThenTo'.
type ReferenceEnumFromThenToArgs a =
  Fcf.Permutations3 @@ '(ReferenceFrom a, ReferenceThen a, ReferenceTo a)

-- | The results of testing 'EnumFromThenTo' against the predefined sets of arguments.
type ReferenceEnumFromThenToResults a =
  Fcf.Map (Fcf.Uncurry3 Fcf.EnumFromThenTo) @@ ReferenceEnumFromThenToArgs a

-- | Some predefined arguments usable for testing 'Previous' and 'Next'.
type ReferencePreviousNextArgs a =
  '[ Start,
     End,
     ReferenceThen a
   ]

-- | The results of testing 'Previous'  against the predefined arguments.
type ReferencePreviousResults a = Fcf.Map Fcf.Previous @@ ReferencePreviousNextArgs a

-- | The results of testing 'Next'  against the predefined arguments.
type ReferenceNextResults a = Fcf.Map Fcf.Next @@ ReferencePreviousNextArgs a

-- | The predefined argument usable for testing 'InhabitantsFrom'.
type ReferenceInhabitantsFromArg a = End Sing.- Plus 10 :: TypeIntegral a

-- | The results of testing 'InhabitantsFrom' against the predefined argument.
type ReferenceInhabitantsFromResults a = Fcf.InhabitantsFrom @@ ReferenceInhabitantsFromArg a

-- | The predefined argument usable for testing 'InhabitantsTo'.
type ReferenceInhabitantsToArg a = Start Sing.+ Plus 10 :: TypeIntegral a

-- | The results of testing 'InhabitantsTo' against the predefined argument.
type ReferenceInhabitantsToResults a = Fcf.InhabitantsTo @@ ReferenceInhabitantsToArg a

-- | The results of testing 'InhabitantsFromTo' against the predefined arguments.
type ReferenceInhabitantsFromToResults a =
  Fcf.Map (Fcf.Uncurry Fcf.InhabitantsFromTo) @@ ReferenceEnumFromToArgs a

-- | An 'Integral' type that has a testable 'TypeIntegral' counterpart.
class (IsTestableIntegral a) => TestableIntegral a where
  -- | Returns the relevant 'Dict's for 'Bounded' types.
  boundedDict ::
    Maybe
      ( Dict
          ( Bounded a,
            Bounded (TypeIntegral a),
            SingI (MinBound :: TypeIntegral a),
            SingI (MaxBound :: TypeIntegral a),
            SingI (ReferenceToFromEnumArgs a),
            SingI (ReferenceToFromEnumResults a)
          )
      )

  -- | Returns the relevant 'Dict's for 'Finitary' types.
  finitaryDict ::
    Maybe
      ( Dict
          ( Finitary a,
            Finitary (TypeIntegral a),
            PFinitary (TypeIntegral a),
            1 <= Cardinality a,
            1 <= Cardinality (TypeIntegral a),
            Cardinality a ~ Cardinality (TypeIntegral a),
            SingI (Start :: TypeIntegral a),
            SingI (End :: TypeIntegral a),
            SingI (ReferencePreviousNextArgs a),
            SingI (ReferencePreviousResults a),
            SingI (ReferenceNextResults a),
            SingI (ReferenceInhabitantsFromArg a),
            SingI (ReferenceInhabitantsFromResults a),
            SingI (ReferenceInhabitantsToArg a),
            SingI (ReferenceInhabitantsToResults a),
            SingI (ReferenceInhabitantsFromToResults a)
          )
      )

instance
  {-# OVERLAPPABLE #-}
  ( Bounded a,
    Finitary a,
    IsTestableIntegral a,
    PFinitary (TypeIntegral a),
    SingI (MinBound :: TypeIntegral a),
    SingI (MaxBound :: TypeIntegral a),
    SingI (ReferenceToFromEnumArgs a),
    SingI (ReferenceToFromEnumResults a),
    1 <= Cardinality a,
    1 <= Cardinality (TypeIntegral a),
    Cardinality a ~ Cardinality (TypeIntegral a),
    SingI (Start :: TypeIntegral a),
    SingI (End :: TypeIntegral a),
    SingI (ReferencePreviousNextArgs a),
    SingI (ReferencePreviousResults a),
    SingI (ReferenceNextResults a),
    SingI (ReferenceInhabitantsFromArg a),
    SingI (ReferenceInhabitantsFromResults a),
    SingI (ReferenceInhabitantsToArg a),
    SingI (ReferenceInhabitantsToResults a),
    SingI (ReferenceInhabitantsFromToResults a)
  ) =>
  TestableIntegral a
  where
  boundedDict = Just Dict
  finitaryDict = Just Dict

instance TestableIntegral Integer where
  boundedDict = Nothing
  finitaryDict = Nothing

-- | A reference 'TypeIntegral' that is smaller than the two others.
type family ReferenceFrom a :: TypeIntegral a where
  ReferenceFrom Integer = Minus 100
  ReferenceFrom _ = Eval (Fcf.UnMaybe Fcf.MinBound (Fcf.Max MinBound) (MinusMaybe 100))

-- | A reference 'TypeIntegral' that lies between the two others.
type family ReferenceThen a :: TypeIntegral a where
  ReferenceThen Integer = Minus 97
  ReferenceThen a = ReferenceFrom a Sing.+ Plus 3

-- | A reference 'TypeIntegral' that is greater than the two others.
type family ReferenceTo a :: TypeIntegral a where
  ReferenceTo Integer = Plus 100
  ReferenceTo _ = Eval (Fcf.UnMaybe Fcf.MaxBound (Fcf.Min MaxBound) (PlusMaybe 100))

-- | A 'Finitary' type that can be tested against another 'Finitary' type.
class (Finitary b, 1 <= Cardinality b, Cardinality a ~ Cardinality b) => TestableFinitary a b

instance (Finitary b, 1 <= Cardinality b, Cardinality a ~ Cardinality b) => TestableFinitary a b

-- | A binary 'Num' operation.
data NumOp a
  = Add a
  | Sub a
  | Mul a
  deriving (Functor, Show)

instance (TestableIntegral a) => Arbitrary (NumOp a) where
  arbitrary = QC.elements [Add, Sub, Mul] <*> arbitrary

-- | A binary 'Integral' operation.
data IntegralOp a
  = BaseOp (NumOp a)
  | Quot a
  | Rem a
  | Div a
  | Mod a
  deriving (Functor, Show)

instance (TestableIntegral a) => Arbitrary (IntegralOp a) where
  arbitrary =
    QC.frequency
      [ (2, BaseOp <$> arbitrary),
        (1, QC.elements [Quot, Rem, Div, Mod] <*> (arbitrary `suchThat` (/= 0)))
      ]

instance (KnownNat n, 1 <= n) => Arbitrary (Finite n) where
  arbitrary = finite <$> QC.chooseInteger (0, (natVal (Proxy @n) `max` 1) - 1)

-- | A variant of `shouldBe` that accounts for exceptions.
--
-- The test passes if both values are equal, or would throw the same exception when evaluated.
shouldBeOrThrowLike :: (HasCallStack, Eq a, Show a) => a -> a -> Expectation
shouldBeOrThrowLike x y = do
  x' <- go x
  y' <- go y
  x' `shouldBe` y'
  where
    go :: a -> IO (Either String a)
    go z = (Right <$> evaluate z) `catch` (\(e :: SomeException) -> pure $ Left $ show e)

-- | Checks whether a 'TypeIntegral' type has the same behavior as the corresponding
-- 'Integral' type when applying a unary operation.
testUnaryOp ::
  forall c a b.
  ( c a,
    c (TypeIntegral a),
    TestableIntegral a,
    Eq b,
    Show b
  ) =>
  (forall d. (c d) => d -> b) ->
  a ->
  Expectation
testUnaryOp f n =
  f (toTypeIntegral n)
    `shouldBe` f n

-- | Checks whether a 'TypeIntegral' type has the same behavior as the corresponding
-- 'Integral' type when applying a unary operation.
testBinaryOp ::
  forall c a b.
  ( c a,
    c (TypeIntegral a),
    TestableIntegral a,
    Eq b,
    Show b
  ) =>
  (forall d. (c d) => d -> d -> b) ->
  (a, a) ->
  Expectation
testBinaryOp f (m, n) =
  f (toTypeIntegral m) (toTypeIntegral n)
    `shouldBe` f m n

-- | Checks whether a 'TypeIntegral' type preserves the isomorphism
-- when applying a unary operation.
testIsomorphism ::
  forall c a.
  ( c a,
    c (TypeIntegral a),
    TestableIntegral a
  ) =>
  (forall b. (c b) => b -> b) ->
  a ->
  Expectation
testIsomorphism f n =
  toIntegral (f (toTypeIntegral n))
    `shouldBe` f n

-- | Checks whether a 'TypeIntegral' type preserves the isomorphism
-- when applying a unary operation in a functorial context.
testIsomorphismF ::
  forall c f a.
  ( c a,
    c (TypeIntegral a),
    TestableIntegral a,
    Functor f,
    Eq (f a),
    Show (f a)
  ) =>
  (forall b. (c b) => b -> f b) ->
  a ->
  Expectation
testIsomorphismF f n =
  (toIntegral <$> f (toTypeIntegral n))
    `shouldBe` f n

-- | Checks whether a 'TypeIntegral' type preserves the isomorphism
-- when applying a nullary operation.
testIsomorphism0 ::
  forall c a.
  ( c a,
    c (TypeIntegral a),
    TestableIntegral a
  ) =>
  (forall b. (c b) => b) ->
  Expectation
testIsomorphism0 f =
  toIntegral (f @(TypeIntegral a))
    `shouldBe` f @a

-- | Checks whether a 'TypeIntegral' type preserves the isomorphism
-- when applying a binary operation.
testIsomorphism2 ::
  forall c a.
  ( c a,
    c (TypeIntegral a),
    TestableIntegral a
  ) =>
  (forall b. (c b) => b -> b -> b) ->
  (a, a) ->
  Expectation
testIsomorphism2 f (m, n) =
  toIntegral (f (toTypeIntegral m) (toTypeIntegral n))
    `shouldBe` f m n

-- | Checks whether a 'TypeIntegral' type preserves the isomorphism
-- when applying a sequence of operations.
testIsomorphismN ::
  forall c f a.
  ( Functor f,
    c a,
    c (TypeIntegral a),
    TestableIntegral a
  ) =>
  (forall b. (c b) => f b -> b -> b) ->
  [f a] ->
  a ->
  Expectation
testIsomorphismN f ops x =
  toIntegral (foldr (f . fmap toTypeIntegral) (toTypeIntegral x) ops)
    `shouldBe` foldr f x ops

-- | Checks whether a 'TypeIntegral' type preserves the isomorphism
-- when applying a binary operation that returns a pair.
testIsomorphismPair ::
  forall c a.
  ( c a,
    c (TypeIntegral a),
    TestableIntegral a
  ) =>
  (forall b. (c b) => b -> b -> (b, b)) ->
  (a, a) ->
  Expectation
testIsomorphismPair f (m, n) =
  bimap toIntegral toIntegral (f (toTypeIntegral m) (toTypeIntegral n))
    `shouldBe` f m n

-- | Checks if an 'Integral' value has a successor.
hasSucc :: forall a. (TestableIntegral a) => a -> Bool
hasSucc n = case boundedDict @a of
  Nothing -> True
  Just Dict -> n < maxBound @a

-- | Checks if an 'Integral' value has a predecessor.
hasPred :: forall a. (TestableIntegral a) => a -> Bool
hasPred n = case boundedDict @a of
  Nothing -> True
  Just Dict -> n > minBound @a

-- | Checks if an 'Integral' value is within the bounds of another 'Integral' type.
withinBoundsOf :: forall a b. (TestableIntegral a, TestableIntegral b) => b -> Bool
withinBoundsOf n = case boundedDict @a of
  Nothing -> True
  Just Dict ->
    let (minInteger, maxInteger) =
          case boundedDict @b of
            Nothing ->
              ( fromIntegral (minBound @a),
                fromIntegral (maxBound @a)
              )
            Just Dict ->
              ( fromIntegral (minBound @a) `max` fromIntegral (minBound @b),
                fromIntegral (maxBound @a) `min` fromIntegral (maxBound @b)
              )
     in (n >= fromIntegral @Integer minInteger)
          && (n <= fromIntegral @Integer maxInteger)

-- | Applies a 'NumOp' to a 'Num'.
applyNumOp :: (Num a) => NumOp a -> a -> a
applyNumOp = \case
  Add n -> (+ n)
  Sub n -> subtract n
  Mul n -> (* n)

-- | Applies an 'IntegralOp' to an 'Integral'.
applyIntegralOp :: (Integral a) => IntegralOp a -> a -> a
applyIntegralOp = \case
  BaseOp op -> applyNumOp op
  Quot n -> (`quot` n)
  Rem n -> (`rem` n)
  Div n -> (`div` n)
  Mod n -> (`mod` n)

-- | A 'Gen'erator for pairs of 'Integral's usable as arguments to 'quot', 'rem', 'div', and 'mod'.
divArgsGen :: (Arbitrary a, Integral a) => Gen (a, a)
divArgsGen = liftA2 (,) arbitrary (arbitrary `suchThat` (/= 0))

-- | A variant of 'uncurry' for 3-tuples.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- | Applies an action given 'Just' some value.
withJust :: (Applicative f) => Maybe a -> (a -> f ()) -> f ()
withJust = flip $ maybe (pure ())
