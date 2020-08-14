{-# LANGUAGE FlexibleInstances #-}
module Substitution(
    Subst(..)
  , Substitutable(..)
  , nullSubst
  , compose
    ) where

import AbsTinyCamiot

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
    apply _ (TFloat a)         = TFloat a
    apply _ (TBool a)          = TBool a

    ftv (TLam _ t1 t2)     = Set.union (ftv t1) (ftv t2)
    ftv (TTup _ types)     = Set.unions (map ftv types)
    ftv (TNil ())          = Set.empty
    ftv (TVar _ var)       = Set.singleton var
    ftv (TAdt _ con types) = Set.unions (map ftv types)
    ftv (TInt _)           = Set.empty
    ftv (TFloat _)         = Set.empty
    ftv (TBool _)          = Set.empty

instance Substitutable (TupType ()) where
    apply s (TTupType a t) = TTupType a (apply s t)

    ftv (TTupType a t) = ftv t

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv   = foldr (Set.union . ftv) Set.empty
