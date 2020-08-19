module Constraint(Constraint(..), Test) where

import AbsTinyCamiot
import PrintTinyCamiot
import Substitution
import TCUtils

import qualified Data.Set as Set

{-
   The constraints issued by the typechecker to be solved by the unifier are
   simply pairs of types. It is up for the unifier to see if there is a
   substitution that will make the two types equal.
-}
type Test = Type () -> Type () -> Maybe TCError
data Constraint = C (Type (), Type (), Maybe Test) | C2 [Constraint]

instance Show Constraint where
    show (C (t1, t2, _)) = "Constraint: " ++ printTree t1 ++ ", " ++ printTree t2
    show (C2 cs) = "Constraint v2: " ++ show cs

instance Substitutable Constraint where
    apply s (C (t1, t2, test)) = C (apply s t1, apply s t2, test)
    apply s (C2 cs)         = C2 (map (apply s) cs)

    ftv (C (t1, t2, _)) = Set.union (ftv t1) (ftv t2)
    ftv (C2 cs)         = Set.unions (map ftv cs)