{-# LANGUAGE RoleAnnotations #-}

module Obgs.Fcf.Constraint where

import Data.Constraint (Constraint)
import Fcf (Exp)
import Prelude (Bool)

type role WhenC phantom phantom phantom

data WhenC :: Bool -> Exp Constraint -> Exp Constraint

type role UnlessC phantom phantom phantom

data UnlessC :: Bool -> Exp Constraint -> Exp Constraint

type role All1C phantom phantom phantom

data All1C :: (a -> Exp Constraint) -> [a] -> Exp Constraint