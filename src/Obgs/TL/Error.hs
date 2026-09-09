{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : $Header$
-- Description : Type utilities for building 'ErrorMessage's.
-- Copyright   : (c) Benoît Leulliette, 2026
-- License     : MIT
-- Maintainer  : benoit.leulliette@gmail.com
-- Stability   : provisional
-- Portability : portable
--
-- Type utilities for building 'ErrorMessage's.
module Obgs.TL.Error
  ( -- * Formatting
    QuoteText,
    ShowOrdinal,
    ShowTypeParened,
    ShowTypeQuoted,
    PprList,
    PprListH,
    PprListV,
    IndentMessage,
    (:$>:),
    (:>>:),

    -- * Testing
    TestMessage,

    -- * Re-exports
    ErrorMessage (..),
    TE.Unsatisfiable,
    TE.unsatisfiable,
    module GHC.TypeError,
  )
where

import Data.Kind (Type)
import Data.Type.Bool (If)
import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeError qualified as TE
import GHC.TypeLits (AppendSymbol, Symbol)
import Obgs.TL.List (Foldr, Foldr1, Length, Map, Replicate)
import Obgs.TL.Nat (Mod, Nat, type (<=?))

-- See the note in 'Obgs.Fcf.Bool' for why the $setup block imports modules it does not use.

-- $setup
-- >>> :m +Obgs.TL.List
-- >>> :m +Obgs.TL.Nat

-- | Quotes a 'Symbol' using the same format as quoted types,
-- for use in an 'ErrorMessage'.
--
-- >>> :kind! QuoteText "abc"
-- ...
-- = Text "`abc'"
type family QuoteText (text :: Symbol) :: ErrorMessage where
  QuoteText text = Text ("`" `AppendSymbol` text `AppendSymbol` "'")

-- | Returns the ordinal suffix corresponding to a given 'Nat',
-- for use in an 'ErrorMessage'.
type family OrdinalSuffix (n :: Nat) :: ErrorMessage where
  OrdinalSuffix 1 = Text "st"
  OrdinalSuffix 2 = Text "nd"
  OrdinalSuffix 3 = Text "rd"
  OrdinalSuffix n = Text "th"

-- | Shows a 'Nat' as an ordinal number, for use in an 'ErrorMessage'.
--
-- >>> :kind! ShowOrdinal 1
-- ...
-- = ShowType 1 :<>: Text "st"
--
-- >>> :kind! ShowOrdinal 4
-- ...
-- = ShowType 4 :<>: Text "th"
type family ShowOrdinal (n :: Nat) :: ErrorMessage where
  ShowOrdinal n = ShowType n :<>: OrdinalSuffix (n `Mod` 10)

-- | Shows a type for use in an 'ErrorMessage', enclosed in parentheses if appropriate.
type family ShowTypeParened (a :: k) :: ErrorMessage where
  ShowTypeParened (f a b c d e g h i) = Text "(" :<>: ShowType (f a b c d e g h i) :<>: Text ")"
  ShowTypeParened (f a b c d e g h) = Text "(" :<>: ShowType (f a b c d e g h) :<>: Text ")"
  ShowTypeParened (f a b c d e g) = Text "(" :<>: ShowType (f a b c d e g) :<>: Text ")"
  ShowTypeParened (f a b c d e) = Text "(" :<>: ShowType (f a b c d e) :<>: Text ")"
  ShowTypeParened (f a b c d) = Text "(" :<>: ShowType (f a b c d) :<>: Text ")"
  ShowTypeParened (f a b c) = Text "(" :<>: ShowType (f a b c) :<>: Text ")"
  ShowTypeParened (f a b) = Text "(" :<>: ShowType (f a b) :<>: Text ")"
  ShowTypeParened (f a) = Text "(" :<>: ShowType (f a) :<>: Text ")"
  ShowTypeParened a = ShowType a

-- | Shows a type for use in an 'ErrorMessage', quoted if appropriate.
type family ShowTypeQuoted a :: ErrorMessage where
  ShowTypeQuoted '[a] = ShowType '[a]
  ShowTypeQuoted (a :: Nat) = ShowType a
  ShowTypeQuoted (a :: Symbol) = ShowType a
  ShowTypeQuoted (_ :: [Type]) = ShowType ('[] :: [Type])
  ShowTypeQuoted a = Text "`" :<>: ShowType a :<>: Text "'"

type ShowTypeQuoted :: k -> ErrorMessage

-- | Pretty-prints a list of types for use in an 'ErrorMessage'.
--
-- The direction is chosen based on the length of the list:
--
--   * horizontal if @('Length' as <= maxH)@,
--   * vertical otherwise.
type family PprList (maxH :: Nat) (as :: [k]) :: ErrorMessage where
  PprList maxH as =
    If
      (Length as <=? maxH)
      (PprListH as)
      (PprListV as)

-- | Pretty-prints a list of types horizontally, for use in an 'ErrorMessage'.
--
-- >>> :kind! PprListH '[]
-- ...
-- = Text "'[]"
--
-- >>> :kind! PprListH '[1]
-- ...
-- = (Text "'[ " :<>: ShowType 1) :<>: Text " ]"
type family PprListH (as :: [k]) :: ErrorMessage where
  PprListH '[] =
    Text "'[]"
  PprListH '[a] =
    Text "'[ " :<>: ShowType a :<>: Text " ]"
  PprListH (a ': as) =
    Text "'[ "
      :<>: ShowType a
      :<>: Foldr1 (:<>:) (Map ((:<>:) (Text ", ")) (Map ShowType as))
      :<>: Text " ]"

-- | Pretty-prints a list of types vertically, for use in an 'ErrorMessage'.
type family PprListV (as :: [k]) :: ErrorMessage where
  PprListV '[] =
    Text "'[]"
  PprListV '[a] =
    Text "'[ " :<>: ShowType a :<>: Text " ]"
  PprListV (a ': as) =
    Text "'[ "
      :<>: ShowType a
      :$$: Foldr1 (:$$:) (Map ((:<>:) (Text " , ")) (Map ShowType as))
      :$$: Text " ]"

-- | Indents an 'ErrorMessage' by a given level (using 2 spaces per level).
--
-- >>> :kind! IndentMessage 1 (Text "x")
-- ...
-- = Text "  " :<>: Text "x"
--
-- >>> :kind! IndentMessage 2 (Text "x")
-- ...
-- = Text "  " :<>: (Text "  " :<>: Text "x")
type family IndentMessage (level :: Nat) (message :: ErrorMessage) :: ErrorMessage where
  IndentMessage 0 message =
    message
  IndentMessage level (top :$$: bottom) =
    IndentMessage level top :$$: IndentMessage level bottom
  IndentMessage level message =
    Foldr (:<>:) message (Replicate level (Text "  "))

-- | A variant of ':$$:' that indents the second 'ErrorMessage' by one level.
type error :$>: error' = error :$$: IndentMessage 1 error'

infixl 5 :$>:

-- | A variant of ':$$:' that indents the second 'ErrorMessage' by two levels.
type error :>>: error' = error :$$: IndentMessage 2 error'

infixl 5 :>>:

-- | A utility type family for testing the output of 'ErrorMessage's.
type family TestMessage (message :: ErrorMessage) where
  TestMessage message = TypeError message
