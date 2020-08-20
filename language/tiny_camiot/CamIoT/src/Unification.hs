module Unification(
    uni
  , uniMany
  , uniEither

  , Solve(..)
  , runSolve
  , solver

  , occursCheck
  , unifyMany
  , unify
  , bind
) where

import AbsTinyCamiot
import PrintTinyCamiot

import Environment
import AstUtils
import Substitution
import Constraint
import TCUtils

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set

-- emit a constraint for unification
uni :: Type () -> Type () -> Maybe Test -> TC ()
uni t1 t2 test = tell [C (t1, t2, test)]

-- Do you want to unify more than two constraints?
-- I convinced myself that if you can unify a and b, and b and c,
-- then you can unify a and c. This SHOULD work! (please)
uniMany :: [Type ()] -> Maybe Test -> TC ()
uniMany []       _ = return ()
uniMany [x]      t = uni x x t -- should be OK
uniMany [x,y]    t = uni x y t
uniMany (x:y:xs) t = uni x y t >> uniMany (y:xs) t

uniEither :: [(Type (), Type ())] -> TC ()
uniEither cs = tell [C2 (map (\(t1,t2) -> C (t1,t2,Nothing)) cs)]

-- TODO change to Identity from IO when done debugging
type Solve a = ExceptT TCError IO a

runSolve :: [Constraint] -> IO (Either TCError Subst)
runSolve constraints = runExceptT (solver st)
  where st = (nullSubst, constraints)

solver :: (Subst, [Constraint]) -> Solve Subst
solver (su, cs) =
    case cs of
        [] -> return su
        (C (t1, t2, mtest):cs') -> do
            su1 <- unify t1 t2
            let newsu = su1 `compose` su

            case mtest of
                (Just test) -> case test (apply newsu t1) (apply newsu t2) of
                    (Just err) -> throwError err
                    _          -> solver (newsu, apply su1 cs')
                Nothing     -> solver (newsu, apply su1 cs')
        (C2 constraints : cs') -> do
            su1 <- tryToUnifyEither constraints
            let newsu = su1 `compose` su
            solver (newsu, apply su1 cs')

tryToUnifyEither :: [Constraint] -> Solve Subst
tryToUnifyEither []                 = return nullSubst
tryToUnifyEither [C (t1, t2, _)]    = unify t1 t2
tryToUnifyEither (C (t1, t2, _):cs) =
    catchError (unify t1 t2) (\e -> tryToUnifyEither cs)

-- Does the free variable we are about to substitute for a type,
-- occur in the new type? E.g we can not substitute a for List a, we
-- will never terminate.
occursCheck :: Substitutable a => Ident -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unifyMany :: [Type ()] -> [Type ()] -> Solve Subst
unifyMany [] [] = return nullSubst
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

unify (TAdt _ con []) (TAdt _ con' [])  | con == con' = return nullSubst
                                        | otherwise   = error "here"
unify (TAdt _ con types) (TAdt _ con' types') | con == con' =
    let doOne subst (t1,t2) = do
            s' <- unify (apply subst t1) (apply subst t2)
            return (s' `compose` subst)
    in foldlM doOne nullSubst (zip types types')

-- variables will just be substituted for the other type
unify (TVar _ var) t = bind var t
unify t (TVar _ var) = bind var t

unify (TInt   ()) (TInt   ()) = return nullSubst
unify (TBool  ()) (TBool  ()) = return nullSubst
unify (TFloat ()) (TFloat ()) = return nullSubst
unify (TNil ()) (TNil ())     = return nullSubst

unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: Ident -> Type () -> Solve Subst
bind a t | t == TVar () a  = return nullSubst -- trying to substitue a for a, doesn't make sense
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = do
             --liftIO $ putStrLn $ "binding " ++ printTree a ++ " to " ++ printTree t
             return $ Map.singleton a t