{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : Type utilities for working with 'Type's.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- Type utilities for working with 'Type's.
module Obgs.TL.Type
  ( -- * Type-level variants
    type ($),
    type (==),
    type (/=),

    -- * Kinds
    KindOf,

    -- * Promoted data constructors
    DataConBaseKind,
    DataConArgs,
  )
where

import Data.Kind (Type)
import Prelude

-- | A type-level variant of '$'.
type family ($) (f :: k -> k') (a :: k) :: k' where
  f $ a = f a

infixr 0 $

-- | A type-level variant of '=='.
type family (==) a b where
  a == a = 'True
  _ == _ = 'False

type (==) :: k -> k' -> Bool

-- | A type-level variant of '/='.
type family (/=) a b where
  a /= a = 'False
  _ /= _ = 'True

type (/=) :: k -> k' -> Bool

-- | Returns the kind of a given type.
type family KindOf (a :: k) :: Type where
  KindOf (a :: k) = k

-- | Returns the "base kind" of a promoted data constructor or value.
-- That is, the corresponding /fully applied/ type constructor.
--
-- >>> :kind! DataConBaseKind ('Just @Int)
-- ...
-- = Maybe Int
--
-- >>> :kind! DataConBaseKind ('(,) 'True 'b')
-- ...
-- = (Bool, Char)
type family DataConBaseKind con where
  -- Examples are based on:
  -- @@@
  -- data Example = NullaryExample | TernaryExample Bool Char Int
  -- @@@

  -- 'TernaryExample :: Int -> Char -> Bool -> Example
  -- `------ᵥ-------ˊ  `-ᵥ-ˊ  `-----------ᵥ-----------ˊ
  --       con           a                g
  DataConBaseKind (con :: a -> g) = DataConBaseKind g
  -- 'TernaryExample :: Int -> Char -> Bool -> Example
  --                           `-ᵥ-ˊ  `-------ᵥ-------ˊ
  --                             a            g
  DataConBaseKind (a -> g) = DataConBaseKind g
  -- 'TernaryExample :: Int -> Char -> Bool -> Example
  --                                          `---ᵥ---ˊ
  --                                              a
  DataConBaseKind (a :: Type) = a
  -- 'NullaryExample :: Example
  -- `------ᵥ-------ˊ  `---ᵥ---ˊ
  --        a              k
  DataConBaseKind (a :: k) = k

type DataConBaseKind :: k -> Type

-- | Returns the types of the arguments that apply to a type constructor.
type family TyConArgs a where
  TyConArgs (a -> r) = a ': TyConArgs r
  TyConArgs a = '[]

type TyConArgs :: k -> [Type]

-- | Returns the types of the arguments that apply to a promoted data constructor.
--
-- >>> :kind! DataConArgs ((:) @Int)
-- ...
-- = [Int, [Int]]
type family DataConArgs con where
  DataConArgs (con :: a -> r) = a ': TyConArgs r
  DataConArgs a = '[]

type DataConArgs :: k -> [Type]