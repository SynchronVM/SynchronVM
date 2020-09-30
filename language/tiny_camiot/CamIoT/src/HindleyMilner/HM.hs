{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-} -- for the concrete example
{-# LANGUAGE FlexibleInstances #-} -- for the concrete example
module HindleyMilner.HM 
       ( 
         Unify(..)
       , runUnify
       ) where

import Parser.AbsTinyCamiot
import Typechecker.Substitution (apply, ftv)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable (foldlM)


{-- | An instance Unify a proof gives a way of unifying elements of type
a where the proof of a successful unification is of type proof.
-}
class Unify a proof where
    
    {-- | The trivial proof. In the case of e.g Type inference, where a proof is a
    map from identifiers to types, this would be the empty substitution.
    -}
    trivial :: proof
    
    -- | Tries to unify a constraint (which is two elements of type a).
    unify   :: a -> a -> Maybe proof
    
    {-- | Refines an element of type a given a proof. Used to e.g apply the
    current substitution to the remaining constraints when type checking.
    -}
    refine  :: proof -> a -> a

    {-- | Combines two proofs. When e.g type checking, a proof is a map from identifiers
    to types. Compose would take the union of applying the first proof to the second
    proof, and the first proof.
    -}
    compose :: proof -> proof -> proof

-- | Convenience instance
instance (Unify a b) => Unify [a] b where
    trivial = trivial @a

    unify [] []         = return $ trivial @a
    unify (a:as) (b:bs) = do
        su1 <- unify @a a b
        su2 <- unify (refine su1 as) (refine su1 bs)
        return $ compose @a su2 su1
    unify _ _ = Nothing

    refine p = map (refine p)

    compose = compose @a @b

{-- | Unify one of the pairs. Simply calling unify and using the list instance will
produce Nothing as soon as a unification fails. This function will, in the case of a
Nothing, discard the Nothing and instead try to unify the next pair.

tl;dr - Instead of propagating a Nothing it keeps trying.
-}
unifyOneOf :: forall a b. Unify a b => [(a,a)] -> Maybe b
unifyOneOf []           = return $ trivial @a
unifyOneOf [(c1,c2)]    = unify c1 c2
unifyOneOf ((c1,c2):cs) = case unify @a @b c1 c2 of
    Just s  -> return s
    Nothing -> unifyOneOf cs

{-- | If you have collected a list of constraints, try to solve them one by one and return
a proof if unification succeeded.
-}
runUnify :: forall a b. Unify a b => [(a,a)] -> Maybe b
runUnify a = runUnify' (a, trivial @a)
  where
      {-- | Tries to unify a list of constraints. If successful, the proof will be
      combined with the 'current' proof su, and the following constraints will be
      refined before they are recursively unified themselves.
      -}
      runUnify' :: forall a b. Unify a b => ([(a,a)], b) -> Maybe b
      runUnify' (cs, su) = case cs of
          []            -> return su
          ((a1,a2):cs') -> do
              su1 <- unify @a @b a1 a2
              let cs'' = map (\(x,y) -> (refine su1 x, refine su1 y)) cs'
              runUnify' (cs'', compose @a @b su1 su)

-- This is code that speaks concretely about our domain, but everything above does not

type Subst = Map.Map Ident Type

instance Unify Type Subst where
    trivial       = Map.empty
    unify         = unify'
    refine        = apply
    compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1


unify' :: Type -> Type -> Maybe Subst
unify' (TLam t1 t2) (TLam t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (refine s1 t2) (refine s1 t2')
    return (compose @Type s2 s1) -- s2 `compose` s1)

unify' (TTup types1) (TTup types2) = unify types1 types2

unify' (TAdt con []) (TAdt con' [])  | con == con' = return $ trivial @Type
                                     | otherwise   = error "here" -- backtrack and write a better error
unify' (TAdt con types) (TAdt con' types') | con == con' =
       foldlM doOne (trivial @Type) (zip types types')
    where
        doOne :: Subst -> (Type, Type) -> Maybe Subst
        doOne subst (t1,t2) = do
            s' <- unify (refine subst t1) (refine subst t2)
            return $ compose @Type s' subst

unify' (TVar var) t = bind var t
unify' t (TVar var) = bind var t

unify' TInt TInt     = return $ trivial @Type
unify' TBool TBool   = return $ trivial @Type
unify' TFloat TFloat = return $ trivial @Type
unify' TNil TNil     = return $ trivial @Type
unify' t1 t2         = Nothing

-- | Returns a substitution where the identifier has been bound to the type.
bind :: Ident -> Type -> Maybe Subst
bind a t | t == TVar a     = return $ trivial @Type
         | occursCheck a t = Nothing
         | otherwise       = return $ Map.singleton a t

occursCheck :: Ident -> Type -> Bool
occursCheck a t = a `Set.member` ftv t