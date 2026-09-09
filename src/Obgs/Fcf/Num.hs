-- |
-- Module      : $Header$
-- Description : First-class families for working with 'Num' types.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- First-class families for working with 'Num' types.
--
-- /All families in this module expect the types involved to have a 'Sing.PNum' instance/.
module Obgs.Fcf.Num
  ( -- * Type-level variants
    type (+),
    type (-),
    type (*),
    Abs,
    Negate,
    Signum,
    FromInteger,
  )
where

import Fcf (Eval, Exp)
import Obgs.TL.Nat (Nat)
import Prelude.Singletons qualified as Sing

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
-- >>> import Fcf.Data.Nat hiding (type (+), type (-), type (*), type (>=))
-- >>> :m +Fcf.Data.Symbol
-- >>> :m +Fcf.Utils
-- >>> import Fcf hiding (type (+), type (-), type (*))
-- >>> import Obgs.TL.Nat (Nat)
-- >>> import Prelude.Singletons ()
--

-- | A type-level variant of '+'.
--
-- >>> :kind! Eval (1 + 2)
-- ...
-- = 3
data (+) :: a -> a -> Exp a

type instance Eval (a + b) = a Sing.+ b

infixl 9 +

-- | A type-level variant of '-'.
--
-- >>> :kind! Eval (3 - 2)
-- ...
-- = 1
data (-) :: a -> a -> Exp a

type instance Eval (a - b) = a Sing.- b

infixl 9 -

-- | A type-level variant of '*'.
--
-- >>> :kind! Eval (3 * 2)
-- ...
-- = 6
data (*) :: a -> a -> Exp a

type instance Eval (a * b) = a Sing.* b

infixl 9 *

-- | A type-level variant of 'abs'.
data Abs :: a -> Exp a

type instance Eval (Abs a) = Sing.Abs a

-- | A type-level variant of 'negate'.
data Negate :: a -> Exp a

type instance Eval (Negate a) = Sing.Negate a

-- | A type-level variant of 'signum'.
--
-- >>> :kind! Signum @@ 3
-- ...
-- = 1
--
-- >>> :kind! Signum @@ 0
-- ...
-- = 0
data Signum :: a -> Exp a

type instance Eval (Signum a) = Sing.Signum a

-- | A type-level variant of 'fromInteger'.
--
-- >>> :kind! (FromInteger @@ 5) :: Nat
-- ...
-- = 5
data FromInteger :: Nat -> Exp a

type instance Eval (FromInteger a) = Sing.FromInteger a
