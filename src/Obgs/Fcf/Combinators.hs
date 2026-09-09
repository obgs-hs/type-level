{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : Useful combinators for working with first-class families.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- Useful combinators for working with first-class families.
module Obgs.Fcf.Combinators
  ( -- * Type-level variants
    type (<&>),
    type (??),
    On,
    Undefined,

    -- * Lifting
    Lift,

    -- * Composition
    type (<$$>),
    type (<.>),
    type (==<<),
    type (<=:<),

    -- * Iteration
    IterateN,
    IterateN_,

    -- * Lifted variants
    ConstM,
    Pure1M,

    -- * Re-exports
    module Fcf.Combinators,
  )
where

import Fcf hiding (type (-))
import Fcf.Class.Functor (FMap)
import Fcf.Combinators
import Obgs.TL.Error qualified as TE
import Obgs.TL.Nat (Nat, type (-))
import Obgs.TL.Type (type (==))

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

-- | A type-level variant of 'Data.Functor.<&>'.
data (<&>) :: Exp a -> (a -> b) -> Exp b

type instance Eval (x <&> f) = f (Eval x)

infixl 1 <&>

-- | A type-level variant of the @flap@ operator.
data (??) :: Exp (a -> b) -> a -> Exp b

type instance Eval (f ?? a) = Eval f a

infixl 4 ??

-- | A type-level variant of 'Data.Function.on'.
data On :: (b -> b -> Exp c) -> (a -> Exp b) -> a -> a -> Exp c

type instance Eval (On f g x y) = Eval (f (g @@ x) (g @@ y))

-- | A type-level variant of 'undefined'.
data Undefined :: Exp a

type instance Eval Undefined = TE.TypeError (TE.Text "Undefined")

-- | Lifts a value into an 'Exp'.
data Lift :: a -> Exp (Exp a)

type instance Eval (Lift x) = Pure x

-- | A combination of 'Fcf.<$>' and 'Fcf.FMap'.
--
-- >>> :kind! Eval (Just <$$> Pure '[1, 2, 3])
-- ...
-- = [Just 1, Just 2, Just 3]
data (<$$>) :: (a -> b) -> Exp (f a) -> Exp (f b)

type instance Eval (f <$$> x) = Eval (FMap (Fcf.Pure1 f) =<< x)

-- | A combination of 'Fcf.<=<' and 'Fcf.<$>'.
--
-- >>> :kind! (Just <.> ((+) 2)) @@ 1
-- ...
-- = Just 3
data (<.>) :: (b -> c) -> (a -> Exp b) -> a -> Exp c

type instance Eval ((f <.> g) x) = f (Eval (g x))

infixl 4 <.>

-- | A type-level combination of 'Fcf.=<<' and nested composition
-- (ie: @(.) . (.)@, most commonly named @.:@).
--
-- >>> :kind! ((+) ==<< Pure 1) @@ 2
-- ...
-- = 3
data (==<<) :: (a -> b -> Exp c) -> Exp a -> b -> Exp c

type instance Eval ((==<<) f x y) = Eval (f (Eval x) y)

infixr 1 ==<<

-- | A type-level combination of 'Fcf.<=<' and nested composition
-- (ie: @(.) . (.)@, most commonly named @.:@).
--
-- >>> :kind! Eval (((+) 3 <=:< (+)) 1 2)
-- ...
-- = 6
data (<=:<) :: (c -> Exp d) -> (a -> b -> Exp c) -> a -> b -> Exp d

type instance Eval ((f <=:< g) x y) = Eval (f (Eval (g x y)))

infixr 1 <=:<

-- | A type-level variant of 'Data.List.iterate' with a fixed number of iterations.
--
-- >>> :kind! IterateN ((+) 1) 0 @@ 5
-- ...
-- = [0, 1, 2, 3, 4, 5]
--
-- >>> :kind! IterateN ((+) 1) 0 @@ 0
-- ...
-- = '[0]
data IterateN :: (a -> Exp a) -> a -> Nat -> Exp [a]

type instance
  Eval (IterateN f x n) =
    x ': (UnBool (IterateN f (f @@ x) (n - 1)) (Pure '[]) @@ (n == 0))

-- | A variant of 'IterateN' that discards intermediate results.
--
-- >>> :kind! IterateN_ ((+) 1) 0 @@ 5
-- ...
-- = 5
--
-- >>> :kind! IterateN_ ((+) 1) 0 @@ 0
-- ...
-- = 0
data IterateN_ :: (a -> Exp a) -> a -> Nat -> Exp a

type instance
  Eval (IterateN_ f x n) =
    UnBool (IterateN_ f (f @@ x) (n - 1)) (Pure x) @@ (n == 0)

-- | A lifted variant of 'ConstFn'.
data ConstM :: Exp a -> b -> Exp a

type instance Eval (ConstM x _) = Eval x

-- | A lifted variant of 'Pure1'.
data Pure1M :: (a -> b) -> Exp (a -> Exp b)

type instance Eval (Pure1M f) = Pure1 f