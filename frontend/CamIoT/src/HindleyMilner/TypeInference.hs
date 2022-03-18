{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module HindleyMilner.TypeInference where

import Parser.AbsTinyCamiot ( Ident, Type(..) )
import Parser.PrintTinyCamiot
import HindleyMilner.HM ( Unify(..) ,UniError(..) )
import Typechecker.Substitution ( Substitutable(ftv, apply) )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable ( foldlM )

type Subst = Map.Map Ident Type

instance Unify Type where
    type Proof Type = Subst
    trivial       = Map.empty
    unify         = unify'
    refine        = apply
    compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1


unify' :: Type -> Type -> Either UniError Subst
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
        doOne :: Subst -> (Type, Type) -> Either UniError Subst
        doOne subst (t1,t2) = do
            s' <- unify (refine subst t1) (refine subst t2)
            return $ compose @Type s' subst

unify' (TVar var) t = bind var t
unify' t (TVar var) = bind var t

unify' TInt TInt     = return $ trivial @Type
unify' TBool TBool   = return $ trivial @Type
unify' TFloat TFloat = return $ trivial @Type
unify' TNil TNil     = return $ trivial @Type
unify' a b           = Left $ UniError $ "cannot unify: " ++ printTree a ++ " with " ++ printTree b


-- | Returns a substitution where the identifier has been bound to the type.
bind :: Ident -> Type -> Either UniError Subst
bind a t  | t == TVar a      = return $ trivial @Type
          | occursCheck a t  = Left $ UniError $ "Occurs check error"
          | otherwise        = return $ Map.singleton a t

occursCheck :: Ident -> Type -> Bool
occursCheck a t = a `Set.member` ftv t
