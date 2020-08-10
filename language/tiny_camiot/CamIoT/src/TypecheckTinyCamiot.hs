{-# LANGUAGE FlexibleInstances #-}
module TypecheckTinyCamiot where

import AbsTinyCamiot
import PrintTinyCamiot

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set

data Scheme = Forall [Ident] (Type ())
newtype TEnv = TEnv (Map.Map Ident Scheme)

extend :: TEnv -> (Ident, Scheme) -> TEnv
extend = undefined

restrict :: TEnv -> Ident -> TEnv
restrict = undefined

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
    apply s (TPair a t1 t2)    = TPair a (apply s t1) (apply s t2)
    apply s (TVar a var)       = Map.findWithDefault (TVar a var) var s
    apply s (TAdt a con types) = TAdt a con (map (apply s) types)
    apply _ (TInt a)           = TInt a
    apply _ (TFloat a)         = TFloat a
    apply _ (TBool a)          = TBool a

    ftv (TLam _ t1 t2)     = Set.union (ftv t1) (ftv t2)
    ftv (TPair _ t1 t2)    = Set.union (ftv t1) (ftv t2)
    ftv (TVar _ var)       = Set.singleton var
    ftv (TAdt _ con types) = Set.unions (map ftv types)
    ftv (TInt _)           = Set.empty
    ftv (TFloat _)         = Set.empty
    ftv (TBool _)          = Set.empty

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

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: TC (Type ())
fresh = do
  s <- get
  put $ s + 1
  return $ TVar () (Ident (letters !! s))

occursCheck :: Substitutable a => Ident -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unify ::  Type () -> Type () -> TC Subst
unify (TLam _ t1 t2) (TLam _ t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return (s2 `compose` s1)

unify (TPair _ t1 t2) (TPair _ t1' t2') = do
    s1 <- unify t1 t1'
    s2 <- unify (apply s1 t2) (apply s1 t2')
    return $ s2 `compose` s1

unify (TAdt _ con []) (TAdt _ con' [])        | con == con' = return nullSubst
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

bind :: Ident -> Type () -> TC Subst
bind a t | t == TVar () a  = return nullSubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ Map.singleton a t

instantiate ::  Scheme -> TC (Type ())
instantiate (Forall vars t) = do
  vars' <- mapM (const fresh) vars
  let s = Map.fromList $ zip vars vars'
  return $ apply s t

generalize :: TEnv -> Type () -> Scheme
generalize env t  = Forall vars t
    where vars = Set.toList $ ftv t `Set.difference` ftv env
{---------------------------------------------------------------------}
{- ******************** -}
-- top level typecheck

typecheck :: [Def ()] -> IO (Either String [Def (Type ())])
typecheck defs = do
    res <- runExceptT (runStateT pgm emptyState)
    case res of
        Left e -> return $ Left (show e)         -- error occured
        Right (defs', _) -> return $ Right defs' -- TC completed
  where pgm = check defs -- the monadic computation that TC's

{- ******************** -}
-- typecheck monad

-- Data type representing all possible errors that can be raised
-- Add variants and needed and include a show instance for it, so that
-- it is rendered nicely.
data TCError =
    {--- Expression errors ---}
    --        exp     actual     expected 
    TypeError (Exp ()) (Type ()) (Type ())
    --         op         left      right
  | RelOpError (RelOp ()) (Type ()) (Type ())
    --         op                             left      right
  | ArithError (Either (AddOp ()) (MulOp ())) (Type ()) (Type ())
    --        left      right
  | BoolError (Type ()) (Type ())
    --          exp      type
  | AppValError (Exp ()) (Type ())

    -- For the hindley milner TC
  | InfiniteType Ident (Type ())
  | UnificationFail (Type ()) (Type ())

instance Show TCError where
    show (TypeError expression act exp) = 
        "Type error ---\n" ++
        "Expected: " ++ show exp ++ "\n" ++
        "Actual: " ++ show act ++ "\n" ++
        "In expression: \n" ++ printTree expression
    show (RelOpError op t1 t2) =
        "Type error ---\n" ++
        "Operator " ++ show op ++ "expect arguments of equal type\n" ++
        "Left  operators type: " ++ show t1 ++ "\n" ++
        "Right operators type: " ++ show t2
    show (ArithError op t1 t2) =
        "Type error ---\n" ++
        "Arithmic operator " ++ show op ++ " expects both arguments to be of the same numerical type\n" ++
        "Left  operators type: " ++ show t1 ++ "\n" ++
        "Right operators type: " ++ show t2
    show (BoolError t1 t2) =
        "Type error ---\n" ++
        "Boolean operators && and || expects both arguments to be of type Bool\n" ++
        "Left  operators type: " ++ show t1 ++ "\n" ++
        "Right operators type: " ++ show t2
    show (AppValError e t) =
        "Type error ---\n" ++
        "Cannot apply " ++ show e ++ " since it does not have a function type\n" ++
        "Actual type: " ++ show t

    show (InfiniteType (Ident var) t) =
        "Type error ---\n" ++
        "Infinite type found during type checking: " ++ var ++ show t

    show (UnificationFail t1 t2) =
        "Type error ---\n" ++
        "Unification failed for types: " ++ show t1 ++ ", " ++ show t2

-- The state kept by the typechecker as it typechecks a program
type TCState = Int

emptyState :: Int
emptyState = 0

-- The typechecking monad! Please change as you see fit. Perhaps we don't
-- want IO in the bottom of it, but i find it usually helps me debug stuff,
-- as you can always just print whatever you want.
type TC a = StateT TCState (ExceptT TCError IO) a

{- ******************** -}
-- typecheck

check :: [Def ()] -> TC [Def (Type ())]
check = undefined

{-
check_ :: Def () -> TC (Def (Type ()))
check_ d = case d of
    DEquation a id patterns body -> undefined
    DTypeSig a id t -> undefined
    DDataDec a name typevars constructors -> undefined

checkExp :: Exp () -> TC (Exp (Type ()))
checkExp e = case e of
    ETup _ tupExps -> do
        -- type the tuple components
        typedTupExps <- mapM (checkExp . deTupExp) tupExps
        -- fetch the types
        let types = map getExpvar typedTupExps
        -- create the tuple type
        let typ = undefined -- TODO TPair etc etc
        -- return the typed tuple expression
        return $ ETup typ (map tupExp typedTupExps)

    ECase _ e1 patterns -> undefined
    ELet _ pattern e1 e2 -> undefined
    ELetR _ pattern e1 e2 -> undefined
    ELam _ pattern e1 -> undefined
    EIf _ e1 e2 e3 -> undefined
    ECon _ constructor exps -> undefined

    EApp _ e1 e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        let t1 = getExpvar te1
        let t2 = getExpvar te2
        case isFunctionType t1 of
            True  -> case canApply t1 t2 of
                Left t    -> return $ EApp t te1 te2
                Right err -> throwError (err e2)
            False -> throwError $ AppValError e1 t1

    EOr _ e1 e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        let t1 = getExpvar te1
        let t2 = getExpvar te2
        case t1 == bool && t2 == bool of
            True  -> return $ EOr bool te1 te2
            False -> throwError $ BoolError t1 t2

    EAnd _ e1 e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        let t1 = getExpvar te1
        let t2 = getExpvar te2
        case t1 == bool && t2 == bool of
            True  -> return $ EAnd bool te1 te2
            False -> throwError $ BoolError t1 t2
    
    ERel _ e1 op e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        let t1 = getExpvar te1
        let t2 = getExpvar te2
        -- TODO how to do Adt? Maybe more involved than we expected
        case t1 == t2 of
            True  -> let optype = function_type [t1, t2] bool
                     in return $ ERel bool te1 (fmap (\_ -> optype) op) te2
            False -> throwError $ RelOpError op t1 t2

    -- Add and Mul are duplicated, could probably generalise
    EAdd _ e1 op e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        let t1 = getExpvar te1
        let t2 = getExpvar te1
        let allowed = [int, float]
        case t1 == t2 && elem t1 allowed of
            True  -> let optype = function_type [t1, t2] t1
                     in return $ EAdd t1 te1 (fmap (\_ -> optype) op) te2
            False -> throwError $ ArithError (Left op) t1 t2

    EMul _ e1 op e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        let t1 = getExpvar te1
        let t2 = getExpvar te1
        let allowed = [int, float]
        case t1 == t2 && elem t1 allowed of
            True  -> let optype = function_type [t1, t2] t1
                     in return $ EMul t1 te1 (fmap (\_ -> optype) op) te2
            False -> throwError $ ArithError (Right op) t1 t2

    ENot _ e1 -> do
        te1 <- checkExp e1
        case getExpvar te1 == bool of
            True  -> return $ ENot bool te1
            False -> throwError $ TypeError e1 (getExpvar te1) bool

    EVar _ var -> undefined -- look up type I guess

    EConst _ const -> return $ case const of
        CInt _ i   -> fmap (\_ -> int) e
        CFloat _ d -> fmap (\_ -> float) e
        CTrue _    -> fmap (\_ -> bool) e
        CFalse _   -> fmap (\_ -> bool) e
        CNil _     -> fmap (\_ -> go "what is this?") e -- TODO
      where go str = TAdt () (UIdent str) []

{- ******************** -}
-- typecheck utility functions

-- map over all definitions and create a type from every
-- data type declaration that is encountered.
collectTypes :: [Def ()] -> TC [Type ()]
collectTypes ds = return $ catMaybes $ map go ds
  where go (DDataDec _ name vars _) = 
           Just $ TAdt () name (map (TVar ()) vars)
        go _ = Nothing

-- Extract the var from a typed expression
getExpvar :: Exp a -> a
getExpvar e = case e of
    ETup a _      -> a
    ECase a _ _   -> a
    ELet a _ _ _  -> a
    ELetR a _ _ _ -> a
    ELam a _ _    -> a
    EIf a _ _ _   -> a
    ECon a _ _    -> a
    EApp a _ _    -> a
    EOr a _ _     -> a
    EAnd a _ _    -> a
    ERel a _ _ _  -> a
    EAdd a _ _ _  -> a
    EMul a _ _ _  -> a
    ENot a _      -> a
    EVar a _      -> a
    EConst a _    -> a

isFunctionType :: Type () -> Bool
isFunctionType (TLam _ _ _) = True
isFunctionType _            = False

-- Can an expression of the first type be applied
-- to an expression of the second type?
-- Returns either the resulting type or a type error
canApply :: Type () -> Type () -> Either (Type ()) (Exp () -> TCError)
canApply (TLam _ t rest) t' = case t == t' of
    True  -> Left rest 
    False -> Right $ \e -> TypeError e t' t

{- ******************** -}
-- ADT related utility functions

tupExp :: Exp a -> TupExp a
tupExp e = ETupExp (getExpvar e) e

deTupExp :: TupExp a -> Exp a
deTupExp (ETupExp _ e) = e

-- synonyms for the built-in types
bool :: Type ()
bool = TAdt () (UIdent "Bool") []

int :: Type ()
int = TAdt () (UIdent "Int") []

float :: Type ()
float = TAdt () (UIdent "Float") []

-- builds a function type from the list of argument types and the result type
function_type :: [Type ()] -> Type () -> Type ()
function_type [] res     = res
function_type (x:xs) res = TLam () x (function_type xs res) -}