{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : First-class families for building conditions and branches.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for building conditions and branches.
module Obgs.Fcf.Branch
  ( -- * Type-level variants
    Otherwise,
    ApplyWhen,
    Guard,
    OtherwiseM,
    GuardM,

    -- * Re-exports
    If,
    Case,
    Match,
    type (-->),
    Is,
    Any,
    Else,
  )
where

import Fcf
import Prelude (Bool (..))

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

-- | A type-level variant of 'Data.Bool.otherwise'.
type Otherwise = 'True

-- | A type-level variant of 'Data.Function.applyWhen'.
--
-- >>> :kind! Eval (ApplyWhen 'True ((+) 2) 1)
-- ...
-- = 3
--
-- >>> :kind! Eval (ApplyWhen 'False ((+) 2) 1)
-- ...
-- = 1
data ApplyWhen :: Bool -> (a -> Exp a) -> a -> Exp a

type instance Eval (ApplyWhen p f x) = If p (f @@ x) x

-- | A type-level equivalent of guards.
--
-- Use 'Otherwise' to provide a default value.
--
-- __The family will get stuck if no branch is matched__.
--
-- >>> :kind! Eval (Guard '[ 'False --> Pure 1, 'True --> Pure 2 ])
-- ...
-- = 2
data Guard :: [Match Bool (Exp a)] -> Exp a

type instance Eval (Guard (('True --> x) ': _)) = Eval x

type instance Eval (Guard (('False --> _) ': cs)) = Eval (Guard cs)

-- | A lifted variant of 'Otherwise'.
type OtherwiseM = Pure 'True

-- | A lifted variant of 'GuardM'.
--
-- Use 'OtherwiseM' to provide a default value.
--
-- __The family will get stuck if no branch is matched__.
--
-- >>> :kind! Eval (GuardM '[ Pure 'False --> Pure 1, Pure 'True --> Pure 2 ])
-- ...
-- = 2
data GuardM :: [Match (Exp Bool) (Exp a)] -> Exp a

type instance Eval (GuardM ((p --> x) ': cs)) = Eval (UnBool (GuardM cs) x =<< p)