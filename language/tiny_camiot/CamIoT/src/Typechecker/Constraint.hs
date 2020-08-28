-- MIT License

-- Copyright (c) 2020 Robert Krook

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
module Typechecker.Constraint(Constraint(..), Test) where

import Parser.AbsTinyCamiot
import Parser.PrintTinyCamiot
import Typechecker.Substitution
import Typechecker.TCUtils

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