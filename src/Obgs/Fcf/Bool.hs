{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : First-class families for working with 'Bool's.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for working with 'Bool's.
--
-- Examples below are based on the following definition:
--
-- @
-- type BoolExps = '[ Pure 'False, Pure 'True ]
-- @
module Obgs.Fcf.Bool
  ( -- * Logical operators
    type (&&^),
    type (||^),

    -- * Conditions
    IfMatching,

    -- * Re-exports
    module Fcf.Data.Bool,
  )
where

import Fcf (Eval, Exp, If, Pure, type (@@))
import Fcf.Data.Bool
import Prelude (Bool (..))

-- [Note]
--
-- The $setup blocks import several modules they make no direct use of.
--
-- GHCi loads a module's type family instances only when that module is imported
-- unqualified. Both 'import M ()' and 'import qualified M' leave the families stuck.
-- 'doctest' brings only the module under test into scope, so without these imports the
-- first expression built on a family such as 'UnBool' comes back unreduced.
--
-- They are load-bearing. Removing them breaks the tests.

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
--
-- >>> :m +Fcf
-- >>> :m +Obgs.Fcf.List
--
-- >>> type BoolExps = '[ Pure 'False, Pure 'True ]

-- | A lifted variant of 'Fcf.Data.Bool.&&'.
--
-- >>> :kind! Eval (FMap (Uncurry (&&^)) =<< CartesianProduct BoolExps BoolExps)
-- ...
-- = [False, False, False, True]
data (&&^) :: Exp Bool -> Exp Bool -> Exp Bool

type instance Eval (a &&^ b) = UnBool (Pure 'False) b @@ Eval a

infixr 3 &&^

-- | A lifted variant of 'Fcf.Data.Bool.||'.
--
-- >>> :kind! Eval (FMap (Uncurry (||^)) =<< CartesianProduct BoolExps BoolExps)
-- ...
-- = [False, True, True, True]
data (||^) :: Exp Bool -> Exp Bool -> Exp Bool

type instance Eval (a ||^ b) = UnBool b (Pure 'True) @@ Eval a

infixr 2 ||^

-- | A variant of 'Fcf.If' based on a lifted predicate.
--
-- >>> :kind! Eval (IfMatching (TyEq 1) (Pure 'True) (Pure 'False) 1)
-- ...
-- = True
--
-- >>> :kind! Eval (IfMatching (TyEq 1) (Pure 'True) (Pure 'False) 0)
-- ...
-- = False
data IfMatching :: (a -> Exp Bool) -> Exp b -> Exp b -> a -> Exp b

type instance Eval (IfMatching p t f x) = Eval (If (p @@ x) t f)