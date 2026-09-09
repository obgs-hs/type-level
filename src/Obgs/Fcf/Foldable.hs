{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : First-class families for working with 'Foldable' types.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for working with 'Foldable' types.
module Obgs.Fcf.Foldable
  ( -- * Type-level variants
    Product,

    -- * Re-exports
    module Fcf.Class.Foldable,
  )
where

import Fcf (Eval, Exp, type (*))
import Fcf.Class.Foldable
import Obgs.TL.Nat (Nat)

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

-- | A type-level variant of 'Data.List.product'.
--
-- >>> :kind! Product @@ '[1, 2, 3]
-- ...
-- = 6
--
-- >>> :kind! Product @@ '[]
-- ...
-- = 1
data Product :: [Nat] -> Exp Nat

type instance Eval (Product ns) = Eval (Foldr (*) 1 ns)