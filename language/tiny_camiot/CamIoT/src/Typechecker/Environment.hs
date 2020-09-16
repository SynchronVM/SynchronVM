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
module Typechecker.Environment
       (
         -- * Type schemes
         Scheme(..)
       , instantiate
       , generalize

         -- * Type checking environment
       , TEnv(..)
       , emptyEnv
       , extend
       , restrict
       , inEnv
       , inEnvMany
       , lookupVar

         -- * Type checking dynamic environment
       , TCState(..)
       , emptyState
       , lookupCons

       , TC()
       , TCError(..)
       , letters
       , fresh
       ) where

import Parser.AbsTinyCamiot ( Type(TVar), UIdent, Ident(..) )
import Parser.PrintTinyCamiot ()
import Typechecker.Substitution
    ( Substitutable(..), Subst, nullSubst, compose )
import Typechecker.Constraint ( Constraint )
import Typechecker.TCUtils ( TCError(..) )

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.State
    ( replicateM, MonadState(put, get), StateT )
import Control.Monad.Reader
    ( replicateM, MonadReader(ask, local), ReaderT )
import Control.Monad.Writer ( replicateM, WriterT )
import Control.Monad.Except
    ( replicateM, MonadError(throwError), ExceptT )

-- | Type schemas, e.g forall a . Maybe a.
data Scheme = Forall [Ident] Type

instance Substitutable Scheme where
    apply s (Forall vars t) = Forall vars $ apply s' t
                              where s' = foldr Map.delete s vars

    ftv (Forall vars t) = ftv t `Set.difference` Set.fromList vars

-- | Type checking environment. Maps identifiers to type schemas.
newtype TEnv = TEnv (Map.Map Ident Scheme)

instance Substitutable TEnv where
  apply s (TEnv env) =  TEnv $ Map.map (apply s) env
  ftv (TEnv env) = ftv $ Map.elems env

-- | Empty environment
emptyEnv :: TEnv
emptyEnv = TEnv Map.empty

-- | Extend the environment with a new identifier and type schema
extend :: TEnv -> (Ident, Scheme) -> TEnv
extend (TEnv env) (x, sc) = TEnv $ Map.insert x sc env

-- | Delete an identifier and type schema from the environment.
restrict :: TEnv -> Ident -> TEnv
restrict (TEnv env) var = TEnv $ Map.delete var env

{-- | Run the type checking computation after extending the local environment with a new
identifier and type schema.
-}
inEnv :: (Ident, Scheme) -> TC a -> TC a
inEnv xsc = inEnvMany [xsc]

{-- | Run the type checking computation after extending the local environment with many
new identifier-type schema pairs.
-}
inEnvMany :: [(Ident, Scheme)] -> TC a -> TC a
inEnvMany xs m = do
    let scope e = foldl (\e' (x,sc) -> restrict e' x `extend` (x, sc)) e xs
    local scope m

-- | Instantiate a type schema with fresh type variables, yielding a unique type.
instantiate ::  Scheme -> TC Type
instantiate (Forall vars t) = do
  vars' <- mapM (const fresh) vars
  let s = Map.fromList $ zip vars vars'
  return $ apply s t

-- | Generalise a polymorphic type to a type schema.
generalize :: TEnv -> Type -> Scheme
generalize env t  = Forall vars t
    where vars = Set.toList $ ftv t `Set.difference` ftv env

-- | Fetch the type of the identifier. If it is a polymorphic type, instantiate it.
lookupVar :: Ident -> TC Type
lookupVar x@(Ident name) = do
    (TEnv env) <- ask
    case Map.lookup x env of
        Just s  -> instantiate s
        Nothing -> throwError $ UnboundVariable name

-- | Fetch the type of the constructor. If it is a polymorphic type, instantiate it.
lookupCons :: UIdent -> TC Type
lookupCons con = do
    env <- get
    case Map.lookup con (constructors env) of
        Just t  -> instantiate t
        Nothing -> throwError $ UnboundConstructor con

-- | Dynamic state to use while typechecking. TODO move constructors into the reader state
data TCState = TCState
    { num            :: Int                    -- ^ Internal state used to generate names
    , constructors   :: Map.Map UIdent Scheme  -- ^ Map of constructors and type schemas
    }

-- | Empty state
emptyState :: TCState
emptyState = TCState 0 Map.empty

-- | The type checking monad (this should be moved somewhere else)
type TC a = ReaderT TEnv (
            WriterT [Constraint] (
            StateT TCState (
            ExceptT TCError 
            IO))) 
            a

-- | Helper function used to generate fresh type variables.
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Generate a fresh type variable.
fresh :: TC Type
fresh = do
  s <- get
  put $ s { num = num s + 1}
  return $ TVar (Ident (letters !! num s))