{-# LANGUAGE FlexibleInstances #-}
module Environment (
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

  , Constraint(..)

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

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except

-- Type scheme
-- e.g id :: a -> a has type scheme forall a . a -> a
data Scheme = Forall [Ident] (Type ())

-- Typing environment, map identifiers to type schemes
newtype TEnv = TEnv (Map.Map Ident Scheme)

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

instantiate ::  Scheme -> TC (Type ())
instantiate (Forall vars t) = do
  vars' <- mapM (const fresh) vars
  let s = Map.fromList $ zip vars vars'
  return $ apply s t

generalize :: TEnv -> Type () -> Scheme
generalize env t  = Forall vars t
    where vars = Set.toList $ ftv t `Set.difference` ftv env

lookupVar :: Ident -> TC (Type ())
lookupVar x@(Ident name) = do
    (TEnv env) <- ask
    case Map.lookup x env of
        Just s  -> instantiate s
        Nothing -> throwError $ UnboundVariable name

lookupCons :: Con () -> TC (Type ())
lookupCons (Constructor () con) = do
    env <- get
    case Map.lookup con (constructors env) of
        Just t  -> instantiate t
        Nothing -> throwError $ UnboundConstructor con

lookupTypeSig :: Ident -> TC (Maybe (Type ()))
lookupTypeSig fun = do
    (TEnv env) <- ask
    let tsig = Map.lookup fun env
    case tsig of
        Just sig -> Just <$> instantiate sig
        Nothing  -> return Nothing

newtype Constraint = C (Type (), Type ())
instance Show Constraint where
    show (C (t1, t2)) = "Constraint: " ++ printTree t1 ++ ", " ++ printTree t2

data TCState = TCState { 
    num  :: Int
  , constructors :: Map.Map UIdent Scheme }

emptyState :: TCState
emptyState = TCState 0 Map.empty

data TCError =
    InfiniteType Ident (Type ())
  | UnificationFail (Type ()) (Type ())
  | UnboundVariable String
  | UnboundConstructor UIdent
  | DuplicateTypeSig Ident
  | DuplicateConstructor UIdent (Type ())
  | TypeArityError UIdent [Type ()] [Type ()] -- Type constructor, expected vars, found vars
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

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: TC (Type ())
fresh = do
  s <- get
  put $ s { num = (num s) + 1}
  return $ TVar () (Ident (letters !! (num s)))

type Subst = Map.Map Ident (Type ())

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

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

instance Substitutable Constraint where
    apply s (C (t1, t2)) = C (apply s t1, apply s t2)
    ftv (C (t1, t2)) = Set.union (ftv t1) (ftv t2)

instance Substitutable Scheme where
    apply s (Forall vars t) = Forall vars $ apply s' t
                              where s' = foldr Map.delete s vars

    ftv (Forall vars t) = ftv t `Set.difference` Set.fromList vars

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TEnv where
  apply s (TEnv env) =  TEnv $ Map.map (apply s) env
  ftv (TEnv env) = ftv $ Map.elems env