module Constraint(Constraint(..)) where

import AbsTinyCamiot
import PrintTinyCamiot
import Substitution

import qualified Data.Set as Set

{-
   The constraints issued by the typechecker to be solved by the unifier are
   simply pairs of types. It is up for the unifier to see if there is a
   substitution that will make the two types equal.
-}
newtype Constraint = C (Type (), Type ())

instance Show Constraint where
    show (C (t1, t2)) = "Constraint: " ++ printTree t1 ++ ", " ++ printTree t2

instance Substitutable Constraint where
    apply s (C (t1, t2)) = C (apply s t1, apply s t2)
    ftv (C (t1, t2)) = Set.union (ftv t1) (ftv t2)