{-# LANGUAGE FlexibleInstances #-}
module Environment (
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

import AbsTinyCamiot
import PrintTinyCamiot
import Substitution
import Constraint

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

{- Type schemes and environments -}
{-*******************************-}
-- Type scheme
-- e.g id :: a -> a has type scheme forall a . a -> a
data Scheme = Forall [Ident] (Type ())

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
instantiate ::  Scheme -> TC (Type ())
instantiate (Forall vars t) = do
  vars' <- mapM (const fresh) vars
  let s = Map.fromList $ zip vars vars'
  return $ apply s t

-- Given a type such as id : a -> a, return a scheme such as
-- forall a . a -> a
generalize :: TEnv -> Type () -> Scheme
generalize env t  = Forall vars t
    where vars = Set.toList $ ftv t `Set.difference` ftv env

-- Look up a type scheme from the environment and instantiate it
lookupVar :: Ident -> TC (Type ())
lookupVar x@(Ident name) = do
    (TEnv env) <- ask
    case Map.lookup x env of
        Just s  -> instantiate s
        Nothing -> throwError $ UnboundVariable name

-- Semantically identical to lookupVar, but it reads from the
-- declared data constructors instead of the declared function
-- signatures. Not sure why this is treated separately, honestly.
-- It does probably not have to be.
lookupCons :: Con () -> TC (Type ())
lookupCons (Constructor () con) = do
    env <- get
    case Map.lookup con (constructors env) of
        Just t  -> instantiate t
        Nothing -> throwError $ UnboundConstructor con

-- If a type signature exists for the function, fetch and instantiate it.
lookupTypeSig :: Ident -> TC (Maybe (Type ()))
lookupTypeSig fun = do
    (TEnv env) <- ask
    let tsig = Map.lookup fun env
    case tsig of
        Just sig -> Just <$> instantiate sig
        Nothing  -> return Nothing
{-*******************************-}

{-
  num: used for generating fresh type variables
  constructors: when we traverse the AST and parse all data type declarations,
                they end up in this map.
-}
data TCState = TCState { 
    num  :: Int
  , constructors :: Map.Map UIdent Scheme }

emptyState :: TCState
emptyState = TCState 0 Map.empty

-- All potential errors that can be thrown. Feel free to
-- add more variants as you need.
-- They don't include much information about the source now, so they can be tricky to read.
data TCError =
    InfiniteType Ident (Type ())
  | UnificationFail (Type ()) (Type ())
  | UnboundVariable String
  | UnboundConstructor UIdent
  | DuplicateTypeSig Ident
  | DuplicateConstructor UIdent (Type ())
  | TypeArityError UIdent [Type ()] [Type ()]
  | WrongConstructorGoal UIdent (Type ()) (Type ())
  | LambdaConstError (Const ())
  | ConstructorNotFullyApplied UIdent Int Int

instance Show TCError where
    show (InfiniteType var t) =
        "Type error ---\n" ++
        "Failed to create the infinite type from type variable " ++ printTree var ++ 
        " with type " ++ printTree t
    show (UnificationFail t1 t2) =
        "Type error ---\n" ++
        "Failed to unify the two types: \n" ++
        printTree t1 ++ " and \n" ++
        printTree t2
    show (UnboundVariable var) =
        "Type error ---\n" ++
        "Unbound variable: " ++ var
    show (UnboundConstructor (UIdent con)) =
        "Type error ---\n" ++
        "Unbound constructor: " ++ show con
    show (DuplicateTypeSig (Ident fun)) =
        "Type error ---\n" ++
        "Type signature for " ++ fun ++ " declared more than once"
    show (DuplicateConstructor c t) =
        "Type error ---\n" ++
        "Data constructor " ++ show c ++ " : " ++ printTree t ++ " declared more than once"
    show (TypeArityError con' tvars vars) =
        "Type error ---\n" ++
        "Arity error - declared type " ++ printTree (TAdt () con' tvars) ++ " does not match " ++
        "inferred type " ++ printTree (TAdt () con' vars)
    show (WrongConstructorGoal con inferred declared) =
        "Type error ---\n" ++
        "Constructor " ++ printTree con ++ " attempts to create a value of type " ++ 
        printTree inferred ++ ", but it has been declared to be of form " ++ printTree declared
    show (LambdaConstError c) =
        "Type error ---\n" ++
        "Lambdas can only abstract over variables, not constants such as " ++ printTree c
    show (ConstructorNotFullyApplied con expected found) =
        "Type error ---\n" ++
        "Data constructor " ++ printTree con ++ " applied to " ++ show found ++ " arguments, " ++
        "but " ++ show expected ++ " is expected"

type TC a = ReaderT TEnv (
            WriterT [Constraint] (
            StateT TCState (
            ExceptT TCError 
            IO))) 
            a

{- type variable generation -}
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: TC (Type ())
fresh = do
  s <- get
  put $ s { num = (num s) + 1}
  return $ TVar () (Ident (letters !! (num s)))