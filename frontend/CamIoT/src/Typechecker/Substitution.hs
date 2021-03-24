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
{-# LANGUAGE RankNTypes #-}
module Typechecker.Substitution
       (
         -- * Substitutions
         Subst(..)
       , Substitutable(..)
       , nullSubst
       , compose
       ) where

import Parser.AbsTinyCamiot ( Type(..), Ident )

import qualified Data.Map as Map
import qualified Data.Set as Set


-- | A substitution is a mapping from identifiers to types
type Subst = Map.Map Ident Type

-- | An empty substitution
nullSubst :: Subst
nullSubst = Map.empty

-- | Compose two substitutions
compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1


class Substitutable a where
    apply :: Subst -> a -> a     -- ^ Apply a substitution
    ftv   :: a -> Set.Set Ident  -- ^ Fetch the free type variables

instance Substitutable Type where
    apply s (TLam t1 t2)     = TLam (apply s t1) (apply s t2)
    apply s (TTup types)     = TTup (map (apply s) types)
    apply _ TNil             = TNil
    apply s (TVar var)       = Map.findWithDefault (TVar var) var s
    apply s (TAdt con types) = TAdt con (map (apply s) types)
    apply _ TInt             = TInt
    apply _ TBool            = TBool
    apply _ TFloat           = TFloat

    ftv (TLam t1 t2)     = Set.union (ftv t1) (ftv t2)
    ftv (TTup types)     = Set.unions (map ftv types)
    ftv TNil             = Set.empty
    ftv (TVar var)       = Set.singleton var
    ftv (TAdt con types) = Set.unions (map ftv types)
    ftv TInt             = Set.empty
    ftv TBool            = Set.empty
    ftv TFloat           = Set.empty

instance Substitutable a => Substitutable [a] where
    apply s = map (apply s)--fmap . apply
    ftv   = foldr (Set.union . ftv) Set.empty
