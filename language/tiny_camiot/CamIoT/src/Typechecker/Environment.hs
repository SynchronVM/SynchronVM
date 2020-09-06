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
module Typechecker.Environment (
    -- TODO now I manually added everything so that it is exported, but when we
    -- are done we should just expose what we need, I suppose..
    Scheme(..)
  , instantiate
  , generalize
  , lookupVar
  , lookupCons
  , lookupTypeSig
  
  , TEnv(..)
  , emptyEnv
  , extend
  , restrict
  , inEnv
  , inEnvMany

  , TC()
  , TCError(..)
  , TCState(..)
  , emptyState
  , letters
  , fresh

  , Subst
  , nullSubst
  , compose
  , Substitutable(..)
    )where

import Parser.AbsTinyCamiot
import Parser.PrintTinyCamiot
import Typechecker.Substitution
import Typechecker.Constraint
import Typechecker.TCUtils

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

{- Type schemes and environments -}
{-*******************************-}
-- Type scheme
-- e.g id :: a -> a has type scheme forall a . a -> a
data Scheme = Forall [Ident] Type

instance Substitutable Scheme where
    apply s (Forall vars t) = Forall vars $ apply s' t
                              where s' = foldr Map.delete s vars

    ftv (Forall vars t) = ftv t `Set.difference` Set.fromList vars

-- Typing environment, map identifiers to type schemes
newtype TEnv = TEnv (Map.Map Ident Scheme)

instance Substitutable TEnv where
  apply s (TEnv env) =  TEnv $ Map.map (apply s) env
  ftv (TEnv env) = ftv $ Map.elems env

emptyEnv :: TEnv
emptyEnv = TEnv Map.empty

extend :: TEnv -> (Ident, Scheme) -> TEnv
extend (TEnv env) (x, sc) = TEnv $ Map.insert x sc env

restrict :: TEnv -> Ident -> TEnv
restrict (TEnv env) var = TEnv $ Map.delete var env

inEnv :: (Ident, Scheme) -> TC a -> TC a
inEnv xsc = inEnvMany [xsc]

-- Extend the local environment with many variables
inEnvMany :: [(Ident, Scheme)] -> TC a -> TC a
inEnvMany xs m = do
    let scope e = foldl (\e' (x,sc) -> restrict e' x `extend` (x, sc)) e xs
    local scope m

-- Given a type scheme, this function will generate fresh
-- names for the bound type variables, replace them and then
-- return the new 'fresh' type.
instantiate ::  Scheme -> TC Type
instantiate (Forall vars t) = do
  vars' <- mapM (const fresh) vars
  let s = Map.fromList $ zip vars vars'
  return $ apply s t

-- Given a type such as id : a -> a, return a scheme such as
-- forall a . a -> a
generalize :: TEnv -> Type -> Scheme
generalize env t  = Forall vars t
    where vars = Set.toList $ ftv t `Set.difference` ftv env

-- Look up a type scheme from the environment and instantiate it
lookupVar :: Ident -> TC Type
lookupVar x@(Ident name) = do
    (TEnv env) <- ask
    case Map.lookup x env of
        Just s  -> instantiate s
        Nothing -> throwError $ UnboundVariable name

-- Semantically identical to lookupVar, but it reads from the
-- declared data constructors instead of the declared function
-- signatures. Not sure why this is treated separately, honestly.
-- It does probably not have to be.
lookupCons :: UIdent -> TC Type
lookupCons con = do
    env <- get
    case Map.lookup con (constructors env) of
        Just t  -> instantiate t
        Nothing -> throwError $ UnboundConstructor con

-- If a type signature exists for the function, fetch and instantiate it.
--lookupTypeSig :: Ident -> TC (Maybe (Type ()))
--lookupTypeSig fun = do
--    (TEnv env) <- ask
--    let tsig = Map.lookup fun env
--    case tsig of
--        Just sig -> Just <$> instantiate sig
--        Nothing  -> return Nothing

lookupTypeSig :: Ident -> TC (Maybe Type)
lookupTypeSig fun = do
    (TCState _ _ e) <- get
    case Map.lookup fun e of
        Just sig -> Just <$> instantiate sig
        Nothing  -> return Nothing

{-*******************************-}

{-
  num: used for generating fresh type variables
  constructors: when we traverse the AST and parse all data type declarations,
                they end up in this map.
-}
data TCState = TCState { 
    num            :: Int
  , constructors   :: Map.Map UIdent Scheme
  , typesignatures :: Map.Map Ident Scheme }

emptyState :: TCState
emptyState = TCState 0 Map.empty Map.empty

type TC a = ReaderT TEnv (
            WriterT [Constraint] (
            StateT TCState (
            ExceptT TCError 
            IO))) 
            a

{- type variable generation -}
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: TC Type
fresh = do
  s <- get
  put $ s { num = num s + 1}
  return $ TVar (Ident (letters !! num s))
