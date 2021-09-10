{- | This module implements the unification machinery that is used during typechecking.
Unification is the act of checking if there is a "Substitution" that when applied to two
types make the types equal.

E.g the following two types can not be unified

@
> unify Int Float
-- error!
@

while these types may be unified

@
unify (Maybe a) (Maybe (Int, Float))
> [a -> (Int, Float)]

unify a b
> [a -> b]

unify (Either a b) c
> [c -> Either a b]
@

-}
{-# LANGUAGE FlexibleContexts #-}
module CamIoT.Typecheck.Unification where

import CamIoT.Internal.Syntax
import CamIoT.Typecheck.Environment
import CamIoT.Typecheck.Substitution
import CamIoT.Typecheck.TCError

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except

{- | Unify two types, possibly returning a substitution.

When we see that one of the types is a type variable, we bind the type variable to the
other type in a substitution. However, before we do this we are performing an occurs
check. The occurs check makes sure than the type variable we are associating with the
other type does not appear in the associated type. E.g

@
unify a (Maybe a)
@

does not work, as we are binding the @a@ to a type that contains @a@. When we apply the
substitution to these two types the first one is never going to get any smaller or
terminate. It will unwind to @Maybe (Maybe (Maybe ...@. -}
unify :: MonadError TCError m => Type -> Type -> m Subst
unify (TTup ts1) (TTup ts2) = unifyMany ts1 ts2

unify (TLam t1 t2) (TLam t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify (apply s1 t2) (apply s1 t2')
  return $ s2 `compose` s1

unify t1@(TAdt uid1 []) t2@(TAdt uid2 []) | uid1 == uid2 = return unitsub
unify (TAdt uid1 ts1) (TAdt uid2 ts2)
  | uid1 == uid2 = unifyMany ts1 ts2 

unify (TVar id) t   = bind id t
unify t (TVar id)   = bind id t
unify TInt TInt     = return unitsub
unify TFloat TFloat = return unitsub
unify TBool TBool   = return unitsub
unify TNil TNil     = return unitsub
unify t1 t2         = throwError $ UnificationError t1 t2

-- | Bind an identifier to a type variable. Perform an occurs check.
bind :: MonadError TCError m => Ident -> Type -> m Subst
bind id t
  | t == TVar id          = return unitsub
  | id `Set.member` ftv t = throwError $ OccursError id t
  | otherwise             = return $ Map.singleton id t

-- | Try to unify many types, pairwise.
unifyMany :: MonadError TCError m => [Type] -> [Type] -> m Subst
unifyMany [] [] = return unitsub
unifyMany [] _ = error ""
unifyMany _ [] = error ""
unifyMany (t1:ts1) (t2:ts2) = do
  sub  <- unify t1 t2
  sub' <- unifyMany (apply sub ts1) (apply sub ts2)
  return $ sub `compose` sub'

{- | Try to unify the first type with at least one type in the second argument.
Tries the candidate types in order and stops as soon as one of them unifies. -}
unifyWithAtleastOne :: MonadError TCError m => Type -> [Type] -> m Subst
unifyWithAtleastOne t []       = error "can not unify with empty list of types"
unifyWithAtleastOne t1 [t2]    = unify t1 t2
unifyWithAtleastOne t1 (t2:ts) = catchError (unify t1 t2) $ \_ -> unifyWithAtleastOne t1 ts

-- | Take a list of types and make sure that they can all be unified with each other
unifyAll :: MonadError TCError m => [Type] -> m Subst
unifyAll []         = return unitsub
unifyAll [t]        = return unitsub
unifyAll (t1:t2:ts) = do
  sub  <- unify t1 t2
  sub' <- unifyAll $ apply sub (t2:ts)
  return $ sub `compose` sub'
