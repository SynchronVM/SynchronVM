module Unification(
    uni
  , uniMany

  , Solve(..)
  , runSolve
  , solver

  , occursCheck
  , unifyMany
  , unify
  , bind
) where

import AbsTinyCamiot
import Environment
import AstUtils

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set

-- emitting constraints
uni :: Type () -> Type () -> TC ()
uni t1 t2 = tell [C (t1, t2)]

uniMany :: [Type ()] -> TC ()
uniMany [] = return ()
uniMany [x] = uni x x -- should be OK
uniMany (x:y:xs) = uni x y >> uniMany xs

-- solving constraints
-- change to Identity from IO when done debugging
type Solve a = StateT Subst (ExceptT TCError IO) a

runSolve :: [Constraint] -> IO (Either TCError Subst)
runSolve constraints = runExceptT (evalStateT (solver st) nullSubst)
  where st = (nullSubst, constraints)

solver :: (Subst, [Constraint]) -> Solve Subst
solver (su, cs) =
    case cs of
        [] -> return su
        (C (t1, t2):cs') -> do
            su1 <- unify t1 t2
            solver (su1 `compose` su, apply su1 cs')

occursCheck :: Substitutable a => Ident -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unifyMany :: [Type ()] -> [Type ()] -> Solve Subst
unifyMany [] [] = return $ nullSubst
unifyMany (t1:ts) (t2:ts') = do
    su1 <- unify t1 t2
    su2 <- unifyMany (apply su1 ts) (apply su1 ts')
    return $ su2 `compose` su1
unifyMany _ _ = error "" -- throwError with a smarter error

unify ::  Type () -> Type () -> Solve Subst
unify (TLam _ t1 t2) (TLam _ t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 `compose` s1)

unify (TTup _ types1) (TTup _ types2) = 
    let types1' = map deTupType types1
        types2' = map deTupType types2
    in unifyMany types1' types2'

unify t1@(TAdt _ con []) t2@(TAdt _ con' [])  | con == con' = return nullSubst
                                              | otherwise   = error "here"
unify (TAdt _ con types) (TAdt _ con' types') | con == con' =
    -- I hope this is right?
    -- I want to unify the first 'pair' of zipped types, and then try to unify
    -- the subsequent pair by first applying the substitution I just got by
    -- unifying the first pair
    foldlM (\s' (t1,t2) -> unify (apply s' t1) (apply s' t2)) nullSubst (zip types types')

unify (TInt ()) (TInt ())     = return nullSubst
unify (TBool ()) (TBool ())   = return nullSubst
unify (TFloat ()) (TFloat ()) = return nullSubst

unify (TVar _ var) t = bind var t
unify t (TVar _ var) = bind var t

unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: Ident -> Type () -> Solve Subst
bind a t | t == TVar () a  = return nullSubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ Map.singleton a t