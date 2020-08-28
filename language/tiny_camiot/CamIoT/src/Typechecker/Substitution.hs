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
{-# LANGUAGE FlexibleInstances #-}
module Typechecker.Substitution(
    Subst(..)
  , Substitutable(..)
  , nullSubst
  , compose
    ) where

import Parser.AbsTinyCamiot

import qualified Data.Map as Map
import qualified Data.Set as Set

-- A substitution is a map from variables to types
-- e.g we have a context with the type signature id : a -> a
-- and an application id 3, a possible substitution is [a -> Int]
type Subst = Map.Map Ident (Type ())

-- empty substitution
nullSubst :: Subst
nullSubst = Map.empty

-- composisng substitutions, building increasingly larger substitutions
compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

-- eg : apply [a -> Int] (Maybe a) => Maybe Int
-- eg : ftv (Either a b)           => {a,b}
class Substitutable a where
    apply :: Subst -> a -> a
    ftv :: a -> Set.Set Ident

instance Substitutable (Type ()) where
    apply s (TLam a t1 t2)     = TLam a (apply s t1) (apply s t2)
    apply s (TTup a types)     = TTup a (map (apply s) types)
    apply _ (TNil a)           = TNil a
    apply s (TVar a var)       = Map.findWithDefault (TVar a var) var s
    apply s (TAdt a con types) = TAdt a con (map (apply s) types)
    apply _ (TInt a)           = TInt a
    apply _ (TBool a)          = TBool a
    apply _ (TFloat a)         = TFloat a

    ftv (TLam _ t1 t2)     = Set.union (ftv t1) (ftv t2)
    ftv (TTup _ types)     = Set.unions (map ftv types)
    ftv (TNil ())          = Set.empty
    ftv (TVar _ var)       = Set.singleton var
    ftv (TAdt _ con types) = Set.unions (map ftv types)
    ftv (TInt a)           = Set.empty
    ftv (TBool a)          = Set.empty
    ftv (TFloat a)         = Set.empty

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv   = foldr (Set.union . ftv) Set.empty
