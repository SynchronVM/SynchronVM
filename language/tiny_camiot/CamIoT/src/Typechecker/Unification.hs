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
module Typechecker.Unification
       (
         -- * Create constraints
         uni
       , uniMany
       , uniEither

         -- * Solve constraints
       , runSolve
       ) where

import Parser.AbsTinyCamiot ( Type(..), Ident )
import Parser.PrintTinyCamiot ()

import Typechecker.Environment(TCError(InfiniteType, UnificationFail), TC )
import Typechecker.Constraint ( Constraint(..), Test )
import Typechecker.Substitution
    ( compose, nullSubst, Subst, Substitutable(..) )

import Control.Monad.Writer ( MonadWriter(tell) )
import Control.Monad.Except
    ( runExceptT, MonadError(throwError, catchError), ExceptT )
import Data.Foldable ( foldlM )
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Emit two types that should be unified
uni :: Type -> Type -> Maybe Test -> TC ()
uni t1 t2 test = tell [C (t1, t2, test)]

{-- | Unify many types. I've (Robert) convinced myself that if you can unify t1 and t2,
and you can unify t2 and t3, then you can surely unify t1 and t3.
-}
uniMany :: [Type] -> Maybe Test -> TC ()
uniMany []       _ = return ()
uniMany [x]      t = uni x x t
uniMany [x,y]    t = uni x y t
uniMany (x:y:xs) t = uni x y t >> uniMany (y:xs) t

{-- | Presented with a list of pairs of types, `uniEither` will try to unify one pair
at a time until one of them succeeds. Used when e.g typechecking `(+)`, which has
the type forall a . a -> a -> a, but must be unified with either Int -> Int -> Int,
or Float -> Float -> Float.
-}
uniEither :: [(Type, Type)] -> TC ()
uniEither cs = tell [C2 (map (\(t1,t2) -> C (t1,t2,Nothing)) cs)]

-- TODO change to Identity from IO when done debugging/implementing. IO not actually needed.
type Solve a = ExceptT TCError IO a

{-- | Given a list of constraints, tries to solve them and returns either an error or a
result in the form of a `Subst`.
-}
runSolve :: [Constraint] -> IO (Either TCError Subst)
runSolve constraints = runExceptT (solver st)
  where st = (nullSubst, constraints)

{-- | A recursive function that receives the current substitution and the remaining
constraints as arguments, and then returns the complete substitution after having
solved all the constraints.
-}
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

{-- | Given a list of constraints, `tryToUnifyEither` will try to unify them one by one,
and while they fail the errors are caught and the next constraint is solved instead.
-}
tryToUnifyEither :: [Constraint] -> Solve Subst
tryToUnifyEither []                 = return nullSubst
tryToUnifyEither [C (t1, t2, _)]    = unify t1 t2
tryToUnifyEither (C (t1, t2, _):cs) =
    catchError (unify t1 t2) (\e -> tryToUnifyEither cs)

{-- | Does the identifier appear in @a@? If it does we have an infinite constraint.
E.g we will never terminate if we try to substitute a for List a.
-}
occursCheck :: Substitutable a => Ident -> a -> Bool
occursCheck a t = a `Set.member` ftv t

-- | Unifies the pairwise types in the argument lists.
unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return nullSubst
unifyMany (t1:ts) (t2:ts') = do
    su1 <- unify t1 t2
    su2 <- unifyMany (apply su1 ts) (apply su1 ts')
    return $ su2 `compose` su1
unifyMany _ _ = error "" -- throwError with a smarter error

-- | Tries to unify two types. Either returns a substitution or throws an error.
unify :: Type -> Type -> Solve Subst
unify (TLam t1 t2) (TLam t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 `compose` s1)

unify (TTup types1) (TTup types2) = unifyMany types1 types2

unify (TAdt con []) (TAdt con' [])  | con == con' = return nullSubst
                                    | otherwise   = error "here" -- backtrack and write a better error
unify (TAdt con types) (TAdt con' types') | con == con' =
    let doOne subst (t1,t2) = do
            s' <- unify (apply subst t1) (apply subst t2)
            return (s' `compose` subst)
    in foldlM doOne nullSubst (zip types types')

unify (TVar var) t = bind var t
unify t (TVar var) = bind var t

unify TInt TInt     = return nullSubst
unify TBool TBool   = return nullSubst
unify TFloat TFloat = return nullSubst
unify TNil TNil     = return nullSubst
unify t1 t2         = throwError $ UnificationFail t1 t2

-- | Returns a substitution where the identifier has been bound to the type.
bind :: Ident -> Type -> Solve Subst
bind a t | t == TVar a     = return nullSubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ Map.singleton a t