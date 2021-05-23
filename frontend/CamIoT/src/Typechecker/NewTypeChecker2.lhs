\documentclass{article}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include spacing.fmt

\long\def\ignore#1{}

\begin{document}

\ignore{
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Typechecker.NewTypeChecker2 where

import Data.Maybe
import Data.Foldable
import Data.List
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Debug

import Control.Applicative hiding (many, some, Const)
import Control.Monad.Combinators.Expr
import Control.Monad.Identity
import Data.Void

import System.IO.Unsafe

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x
\end{code}
}

\begin{code}
data Type = TVar Ident
          | TInt
          | TFloat
          | TBool
          | TNil
          | TTup [Type]
          | TAdt UIdent [Type]
          | TLam Type Type
  deriving (Eq, Show)

args :: Type -> [Type]
args (TLam t1 t2) = t1 : args t2
args _            = []

construction :: Type -> Type
construction (TLam t1 t2) = construction t2
construction t            = t

funtype :: [Type] -> Type
funtype [t]        = t
funtype (t1:t2:ts) = TLam t1 (funtype (t2:ts))

data Ident = Ident String
  deriving (Eq, Show, Ord)

data UIdent = UIdent String
  deriving (Eq, Show, Ord)

data Pat a = PVar a Ident
           | PNil a
           | PConst a Lit
           | PWild a
           | PAs a Ident (Pat a)
           | PAdt a UIdent [Pat a]
           | PTup a [Pat a]
  deriving (Eq, Show)

patVar :: Pat a -> a
patVar p = case p of
  PVar a _   -> a
  PNil a     -> a
  PConst a _ -> a
  PWild a    -> a
  PAs a _ _  -> a
  PAdt a _ _ -> a
  PTup a _   -> a

data Lit = LInt Int
         | LFloat Double
         | LBool Bool
         | LNil
  deriving (Eq, Show)

data Exp a = EVar a Ident
           | ECon a UIdent
           | ELit a Lit
           | ECase a (Exp a) [(Pat a, Exp a)]
           | ETup a [(Exp a)]
           | EBin a (Exp a) (Exp a) (Binop a)
           | EUn a (Exp a) (Unop a)
           | ELam a (Pat a) (Exp a)
           | EApp a (Exp a) (Exp a)
           | ELet a (Pat a) (Exp a) (Exp a)
           | EIf a (Exp a) (Exp a) (Exp a)
  deriving (Eq, Show)

expVar :: Exp a -> a
expVar e = case e of
  EVar a _     -> a
  ECon a _     -> a
  ELit a _     -> a
  ECase a _ _  -> a
  ETup a _     -> a
  EBin a _ _ _ -> a
  EUn a _ _    -> a
  ELam a _ _   -> a
  EApp a _ _   -> a
  ELet a _ _ _ -> a
  EIf a _ _ _  -> a

data Binop a = Add a
             | Sub a
             | Mul a
             | Div a
             | OLT a
             | OLE a
             | OGT a
             | OGE a
             | OEQ a
             | And a
             | Or a
  deriving (Eq, Ord, Show)

binopVar :: Binop a -> a
binopVar op = case op of
  Add a -> a
  Sub a -> a
  Mul a -> a
  Div a -> a
  OLT a -> a
  OLE a -> a
  OGT a -> a
  OGE a -> a
  OEQ a -> a
  And a -> a
  Or  a -> a

setBinopVar :: a -> Binop b -> Binop a
setBinopVar a op = case op of
  Add _ -> Add a
  Sub _ -> Sub a
  Mul _ -> Mul a
  Div _ -> Div a
  OLT _ -> OLT a
  OLE _ -> OLE a
  OGT _ -> OGT a
  OGE _ -> OGE a
  OEQ _ -> OEQ a
  And _ -> And a
  Or _  -> Or a

data Unop a = Not a
  deriving (Eq, Show, Ord)

unopVar :: Unop a -> a
unopVar op = case op of
  Not a -> a

setUnopVar :: a -> Unop b -> Unop a
setUnopVar a op = case op of
  Not _ -> Not a

data Def a = DTypeSig Ident Type
           | DEquation a Ident [Pat a] (Exp a)
           | DDataDec UIdent [Ident] [(UIdent, Type)]
  deriving (Eq, Show)

getName :: Def a -> Ident
getName (DTypeSig id _)      = id
getName (DEquation _ id _ _) = id

defVar :: Def a -> Maybe a
defVar d = case d of
  DTypeSig _ _      -> Nothing
  DEquation a _ _ _ -> Just a

data Function a = Function
  { name      :: Ident
  , equations :: [Def a]
  , typesig   :: Maybe Type
  }
  deriving (Eq)

instance Print a => Show (Function a) where
  show = printTree

data Program a = Program
  { datatypes :: [ADT]
  , functions :: [Function a]
  , main      :: Function a
  }

type ADT = (UIdent, [Ident], [(UIdent, Type)])

instance Print a => Show (Program a) where
  show = printTree

mkProgram :: Eq a => [Def a] -> Program a
mkProgram defs =
  let (datadecs,funs)    = partitionDataDecs defs
      functions          = mkFunctions funs
      (main, functions') = partitionMain functions
  in Program (map unwrapDataDec datadecs) functions' main
  where
      unwrapDataDec (DDataDec uid vars cons) = (uid, vars, cons)

partitionDataDecs :: [Def a] -> ([Def a], [Def a])
partitionDataDecs ds = partition pred ds
  where
      pred d = case d of
        DDataDec _ _ _ -> True
        _              -> False

mkFunctions :: [Def a] -> [Function a]
mkFunctions defs = map toFunction $ groups defs

partitionMain :: Eq a => [Function a] -> (Function a, [Function a])
partitionMain funs = let main  = find ((==) (Ident "main") . name) funs
                         funs' = delete (fromJust main) funs
                     in (fromJust main, funs')

pred :: Def a -> Def a -> Bool
pred d1 d2 = getName d1 == getName d2

groups :: [Def a] -> [[Def a]]
groups defs = groupBy Typechecker.NewTypeChecker2.pred defs

-- assuming a singleton list is a DEquation, and that there are no
-- type signatures without equations
toFunction :: [Def a] -> Function a
toFunction [x]    = Function (getName x) [x] Nothing
toFunction (d:ds) = case d of
  DTypeSig id t -> Function id ds (Just t)
  -- In this case there was no declared type signature
  _             -> Function (getName d) (d:ds) Nothing

compile :: String -> IO (Either String (Program Type, Subst))
compile fp = do
  contents <- TIO.readFile fp
  let processed = process contents
  let parsed    = parse pProgram fp processed
  case parsed of
    Left e  -> error $ show e
    Right r -> typecheck r

tryParse :: String -> IO String
tryParse fp = do
  contents      <- TIO.readFile fp
  let processed = process contents
  let parsed    = parse pProgram fp processed
  case parsed of
    Left e  -> return $ show e
    Right t -> return $ printTree t

type Subst = Map.Map Ident Type

unitsub :: Subst
unitsub = Map.empty

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set Ident -- free type variables

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)
  ftv     = foldr (Set.union . ftv) Set.empty

-- Left biased substitution
compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

instance Substitutable Type where
  apply s t = case t of
    TVar id     -> Map.findWithDefault (TVar id) id s
    TInt        -> TInt
    TFloat      -> TFloat
    TBool       -> TBool
    TNil        -> TNil
    TLam t1 t2  -> TLam (apply s t1) (apply s t2)
    TAdt uid ts -> TAdt uid (apply s ts)
    TTup ts     -> TTup $ map (apply s) ts

  ftv t = case t of
    TVar id     -> Set.singleton id
    TInt        -> Set.empty
    TFloat      -> Set.empty
    TBool       -> Set.empty
    TNil        -> Set.empty
    TLam t1 t2  -> Set.union (ftv t1) (ftv t2)
    TAdt uid ts -> Set.unions $ map ftv ts
    TTup ts     -> Set.unions $ map ftv ts

instance Substitutable (Pat Type) where
  apply s p = case p of
    PVar a id     -> PVar (apply s a) id
    PNil a        -> PNil (apply s a)
    PConst a l    -> PConst (apply s a) l
    PWild a       -> PWild (apply s a)
    PAs a id p    -> PAs (apply s a) id (apply s p)
    PAdt a uid ps -> PAdt (apply s a) uid $ map (apply s) ps
    PTup a ps     -> PTup (apply s a) $ map (apply s) ps

  -- We will never actually need this function for patterns, or anything other
  -- than types for that matter.
  ftv p = undefined

instance Substitutable (Exp Type) where
  apply s e = case e of
    EVar a id      -> EVar (apply s a) id
    ECon a uid     -> ECon (apply s a) uid
    ELit a l       -> ELit (apply s a) l
    ECase a e pms  -> ECase a (apply s e) $ map (\(p,e) -> (apply s p, apply s e)) pms
    ETup a es      -> ETup (apply s a) $ map (apply s) es
    EBin a e1 e2 o -> EBin (apply s a) (apply s e1) (apply s e2) (apply s o)
    EUn a e o      -> EUn (apply s a) (apply s e) (apply s o)
    ELam a p e     -> ELam (apply s a) (apply s p) (apply s e)
    EApp a e1 e2   -> EApp (apply s a) (apply s e1) (apply s e2)
    ELet a p e1 e2 -> ELet (apply s a) (apply s p) (apply s e1) (apply s e2)
    EIf a e1 e2 e3 -> EIf (apply s a) (apply s e1) (apply s e2) (apply s e3)

  ftv e = undefined

instance Substitutable (Binop Type) where
  apply s op = case op of
    Add a -> Add $ apply s a
    Sub a -> Sub $ apply s a
    Mul a -> Mul $ apply s a
    Div a -> Div $ apply s a
    OLT a -> OLT $ apply s a
    OLE a -> OLE $ apply s a
    OGT a -> OGT $ apply s a
    OGE a -> OGE $ apply s a
    OEQ a -> OEQ $ apply s a
    And a -> And $ apply s a
    Or  a -> Or  $ apply s a

  ftv op = undefined

instance Substitutable (Unop Type) where
  apply s op = case op of
    Not a -> Not $ apply s a

  ftv op = undefined

instance Substitutable (Def Type) where
  apply s d = case d of
    DTypeSig id t         -> DTypeSig id t
    DEquation t id args e -> DEquation (apply s t) id (map (apply s) args) (apply s e)

  ftv = undefined

instance Substitutable (Function Type) where
  apply s f = f { equations = map (apply s) (equations f)
                , typesig   = maybe Nothing (Just . apply s) (typesig f)
                }

  ftv = undefined

instance Substitutable (Program Type) where
  apply s p = p { functions = map (apply s) (functions p)
                , main      = apply s (main p)
                }

  ftv = undefined

refine :: [Function Type] -> Subst -> [Function Type]
refine funs subst = map (apply subst) funs

unify :: Type -> Type -> TC Subst
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

bind :: Ident -> Type -> TC Subst
bind id t
  | t == TVar id          = return unitsub
  | id `Set.member` ftv t = throwError $ OccursError id t
  | otherwise             = return $ Map.singleton id t

unifyMany :: [Type] -> [Type] -> TC Subst
unifyMany [] [] = return unitsub
unifyMany [] _ = error ""
unifyMany _ [] = error ""
unifyMany (t1:ts1) (t2:ts2) = do
  sub  <- unify t1 t2
  sub' <- unifyMany (apply sub ts1) (apply sub ts2)
  return $ sub `compose` sub'

unifyWithAtleastOne :: Type -> [Type] -> TC Subst
unifyWithAtleastOne t []       = error "can not unify with empty list of types"
unifyWithAtleastOne t1 [t2]    = unify t1 t2
unifyWithAtleastOne t1 (t2:ts) = catchError (unify t1 t2) $ \_ -> unifyWithAtleastOne t1 ts

unifyAll :: [Type] -> TC Subst
unifyAll []         = return unitsub
unifyAll [t]        = return unitsub
unifyAll (t1:t2:ts) = do
  sub  <- unify t1 t2
  sub' <- unifyAll $ apply sub (t2:ts)
  return $ sub `compose` sub'

data Schema = Forall [Ident] Type
  deriving Show

instance Substitutable Schema where
  -- Since the variable in `vars` are bound by the forall any mapping involving
  -- them in the substitution must be removed before the substitution is applied.
  apply s (Forall vars t) = Forall vars $ apply s' t
    where s' = foldr Map.delete s vars

  ftv (Forall vars t) = ftv t `Set.difference` Set.fromList vars

data Env = Env (Map.Map Ident Schema) (Map.Map UIdent Schema)
  deriving Show

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

instance Substitutable Env where
  apply s (Env m1 m2) = Env (Map.map (apply s) m1) m2

  -- Not sure we need to account for the FTV in the constructors, they will
  -- be constant
  ftv (Env m1 m2) = ftv $ Map.elems m1

generalize :: Type -> Env -> Schema
generalize t env = Forall vars t
  where
      vars :: [Ident]
      vars = Set.toList $ ftv t `Set.difference` ftv env

data TCError = UnboundVariable Ident
             | UnknownConstructor UIdent
             | UnknownBinop (Binop ())
             | OccursError Ident Type
             | UnificationError Type Type
             | UndeclaredTycon UIdent
             | DuplicateTycon UIdent
             | DuplicateDataConstructor UIdent
             | PartiallyAppliedTycon UIdent Int Int
             | UnboundADTVariable UIdent [Ident] UIdent Type Ident
             | NonADTConstruction UIdent Type Type

instance Show TCError where
  show e = case e of
    UnboundVariable id     -> "can not resolve symbol: " ++ show id
    UnknownConstructor uid -> "can not resolve constructor: " ++ show uid
    UnknownBinop op        -> "can not resolve binop: " ++ printTree op
    OccursError id t       ->
      concat ["Can not substitute ", show id
             , " for ", show t
             , "! the type does not become more concrete."
             ]
    UnificationError t1 t2 ->
      concat ["Can not unify the two types "
             , show t1, " and ", show t2
             ]
    UndeclaredTycon uid    -> concat ["Undeclared type constructor: ", printTree uid]
    DuplicateTycon uid     -> concat ["Type constructor ", printTree uid, " already declared"]
    DuplicateDataConstructor uid -> concat ["Data constructor ", printTree uid, " already declared"]
    PartiallyAppliedTycon uid expectedarity actualarity ->
      concat [ "Type constructor ", printTree uid, " is partially applied; expected arity is "
             , show expectedarity, " but actual arity is ", show actualarity]
    UnboundADTVariable tycon vars datacon t unexpected ->
      concat [ "The data constructor ", printTree datacon, " declared with "
             , printTree tycon, " ", printTree vars, ", is declared to have type ", printTree t
             , ", but the data declaration only binds variables [", printTree vars, "]. The "
             , "variable ", printTree unexpected, " is unbound and unexpected"]
    NonADTConstruction datacon expected actual ->
      concat [ "The data constructor ", printTree datacon, " constructs a value of type "
             , printTree actual, ", but the expected type is ", printTree expected]

data TCState = TCState { namegen :: Int
                       , tycons  :: Map.Map UIdent Int  -- ^ ADT arity
                       }

type TC a = ExceptT TCError (
            StateT TCState (
            ReaderT Env IO))
            a

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: TC Type
fresh = do
  s <- get
  put $ s { namegen = namegen s + 1}
  return $ TVar (Ident (letters !! namegen s))

-- Notice that if the Schema doesn't contain any free variables the result of this
-- function is just the type `t` in `Forall vars t`, unchanged
instantiate :: Schema -> TC Type
instantiate (Forall vars t) = do
  freshtypevars <- mapM (const fresh) vars
  let s = Map.fromList $ zip vars freshtypevars
  return $ apply s t

lookupVar :: Ident -> TC Type
lookupVar id = do
  (Env env _) <- ask
  case Map.lookup id env of
    Just schema -> instantiate schema
    Nothing     -> throwError $ UnboundVariable id

lookupCon :: UIdent -> TC Type
lookupCon uid = do
  (Env _ env) <- ask
  case Map.lookup uid env of
    Just schema -> instantiate schema
    Nothing -> throwError $ UnknownConstructor uid

-- | Does a constructor already exist? Used for verifying data type declarations,
-- that the constructors are unique.
existsCon :: UIdent -> TC Bool
existsCon uid = do
  (Env _ env) <- ask
  case Map.lookup uid env of
    Just _  -> return True
    Nothing -> return False 

lookupTyconArity :: UIdent -> TC Int
lookupTyconArity uid = do
  st <- get
  case Map.lookup uid (tycons st) of
    Just i  -> return i
    Nothing -> throwError $ UndeclaredTycon uid

extendTyconArity :: UIdent -> Int -> TC ()
extendTyconArity uid arity = do
  st <- get
  case Map.lookup uid (tycons st) of
    Just _ -> throwError $ undefined
    Nothing -> put $ st { tycons = Map.insert uid arity (tycons st) }

lookupBinop :: Binop () -> TC Type
lookupBinop op =
  case Map.lookup op binoptypes of
    Just schema -> instantiate schema
    Nothing     -> throwError $ UnknownBinop op

binoptypes :: Map.Map (Binop ()) Schema
binoptypes = Map.fromList $
     [ (Add (), Forall [a] $ TLam ta (TLam ta ta))
     , (Sub (), Forall [a] $ TLam ta (TLam ta ta))
     , (Mul (), Forall [a] $ TLam ta (TLam ta ta))
     , (Div (), Forall [a] $ TLam ta (TLam ta ta))
     , (OLT (), Forall [a] $ TLam ta (TLam ta TBool))
     , (OLE (), Forall [a] $ TLam ta (TLam ta TBool))
     , (OGT (), Forall [a] $ TLam ta (TLam ta TBool))
     , (OGE (), Forall [a] $ TLam ta (TLam ta TBool))
     , (OEQ (), Forall [a] $ TLam ta (TLam ta TBool))
     , (And (), Forall [] $ TLam TBool (TLam TBool TBool))
     , (Or  (), Forall [] $ TLam TBool (TLam TBool TBool))
     ]
  where
     a :: Ident
     a = Ident "a"

     ta :: Type
     ta = TVar a

binopCandidates :: Binop () -> [Type]
binopCandidates op = case op of
  Add _ -> [intintint, floatfloatfloat]
  Sub _ -> [intintint, floatfloatfloat]
  Mul _ -> [intintint, floatfloatfloat]
  Div _ -> [floatfloatfloat]
  OLT _ -> [intintbool, floatfloatbool]
  OLE _ -> [intintbool, floatfloatbool]
  OGT _ -> [intintbool, floatfloatbool]
  OGE _ -> [intintbool, floatfloatbool]
  OEQ _ -> [intintbool, floatfloatbool, boolboolbool]
  And _ -> [boolboolbool]
  Or  _ -> [boolboolbool]
  where
    intintint       = TLam TInt   (TLam TInt   TInt)
    floatfloatfloat = TLam TFloat (TLam TFloat TFloat)
    intintbool      = TLam TInt   (TLam TInt   TBool)
    floatfloatbool  = TLam TFloat (TLam TFloat TBool)
    boolboolbool    = TLam TBool  (TLam TBool  TBool)

lookupUnop :: Unop () -> TC Type
lookupUnop op = undefined

unoptypes :: Map.Map (Unop ()) Schema
unoptypes = Map.fromList $
  [ (Not (), Forall [] $ TLam TBool TBool)
  ]

unopCandidates :: Unop () -> [Type]
unopCandidates op = case op of
  Not () -> [TLam TBool TBool]

extend :: Env -> Ident -> Schema -> Env
extend (Env m1 m2) id schema = Env (Map.insert id schema m1) m2

restrict :: Env -> Ident -> Env
restrict (Env m1 m2) id = Env (Map.delete id m1) m2

inEnv :: Ident -> Schema -> TC a -> TC a
inEnv id schema ma = inEnvMany [(id,schema)] ma

inEnvMany :: [(Ident, Schema)] -> TC a -> TC a
inEnvMany xs ma =
  let scope e = foldl (\e' (id,schema) -> extend (restrict e' id) id schema) e xs
  in local scope ma

withConstructors :: [ADT] -> TC a -> TC a
withConstructors adts = local (\(Env m1 m2) -> Env m1 $ Map.fromList allConsSchemas)
  where
      adtToCons :: ADT -> [(UIdent, Schema)]
      adtToCons (_,vars,cons) = map (\(con,t) -> (con, Forall vars t)) cons

      allConsSchemas :: [(UIdent, Schema)]
      allConsSchemas = concat $ map adtToCons adts

-- | Can use this for type signatures to make sure all types are
-- fully applied.
containsFullyAppliedTycons :: Type -> TC ()
containsFullyAppliedTycons t = case t of
  TAdt uid ts -> do
    arity <- lookupTyconArity uid
    if arity == length ts
      then return ()
      else throwError $ PartiallyAppliedTycon uid arity (length ts)

  TTup ts     -> mapM_ containsFullyAppliedTycons ts
  TLam t1 t2  -> containsFullyAppliedTycons t1 >> containsFullyAppliedTycons t2
  _           -> return ()

whenM :: Monad m => m Bool -> m a -> a -> m a
whenM mb ma d = do
  b <- mb
  if b
    then ma
    else return d

checkPat :: Pat () -> TC (Pat Type)
checkPat p = case p of
  PVar () id    -> do
    t <- fresh
    return $ PVar t id
  PNil () -> return $ PNil TNil
  PConst () l   ->
    return $ PConst (constType l) l
  PWild () -> do
    t <- fresh
    return $ PWild t
  PAs () id p -> do
    p' <- checkPat p
    return $ PAs (patVar p') id p'
  PAdt () uid ps -> undefined
  PTup () ps -> do
    ps' <- mapM checkPat ps
    let tuptype = TTup $ map patVar ps'
    return $ PTup tuptype ps'

constType :: Lit -> Type
constType (LInt _)   = TInt
constType (LFloat _) = TFloat
constType (LBool _)  = TBool
constType LNil       = TNil

patBindings :: Pat Type -> [(Ident, Type)]
patBindings p = case p of
  PVar t id     -> [(id,t)]
  PNil _        -> []
  PConst _ l    -> []
  PWild _       -> []
  PAs t id p    -> (id, t) : patBindings p
  PAdt _ uid ps -> concat $ map patBindings ps
  PTup _ ps     -> concat $ map patBindings ps

checkPatAndBindings :: Pat () -> TC (Pat Type, [(Ident, Type)])
checkPatAndBindings p = do
  p' <- checkPat p
  let bindings = patBindings p'
  return (p', bindings)

checkDef :: Maybe Type -> Def () -> TC (Subst, Def Type)
checkDef mt d = case d of
  DTypeSig id t             -> return $ (unitsub, DTypeSig id t)
  DEquation () id pargs body -> do
    -- annotate arguments with type information
    args'  <- mapM checkPat pargs
    
    -- if there was a declared type signature, create a substitution
    -- that unifies the annotated argument types with it
    patsub <- case mt of
      Just tsig -> do env       <- ask
                      let tsig' = generalize tsig env
                      instsig   <- instantiate tsig'
                      unifyMany (map patVar args') (args instsig)
      Nothing   -> return unitsub
    
    -- annotate the argument types
    let args''      = apply patsub args'
    -- fetch the declared variables and their types from the arguments
    let argbindings = concat $ map patBindings args''
    -- convert them to schemas, to extend the environment with
    let argschemas  = map (\(id,t) -> (id, Forall [] t)) argbindings
    -- typecheck the equation body
    (sub, body')    <- inEnvMany argschemas $ checkExp body
    -- create the inferred type of the entire definition
    let functype    = foldr TLam (expVar body') $ map patVar args''
    return (patsub `compose` sub, DEquation functype id args'' body')

checkFunction :: Function () -> TC (Subst, Function Type)
checkFunction f = do
  -- typecheck equations and fetch their types
  subneqs         <- mapM (checkDef (typesig f)) $ equations f
  let (subs, eqs) = unzip subneqs
  let types       = map (fromJust . defVar) $ filter (isJust . defVar) eqs
  
  let sub = foldl compose unitsub subs

  -- unify all the types of the equations and with the typesig, if
  -- one exists
  sub' <- unifyEquations (maybe types (: types) (typesig f)) sub
  
  -- return annotated functions
  return $ (sub', f { equations = eqs
                    , typesig   = maybe (Just (head types)) Just (typesig f)
                    }
           )
  where
      unifyEquations :: [Type] -> Subst -> TC Subst
      unifyEquations []  s      = return s
      unifyEquations [x] s      = return s
      unifyEquations (x:y:xs) s = do
        s' <- unify x y
        unifyEquations (y:xs) (s `compose` s')

-- | Check that the declaration of an ADT is okay.
checkDataDeclaration :: ADT -> TC ()
checkDataDeclaration (tycon, vars, cons) = do
  extendTyconArity tycon (length vars)
  let constructors = map fst cons

  -- make sure that each constructor is okay
  forM_ cons $ \(con,t) -> do

    -- was this constructor already declared with another ADT?
    whenM (existsCon con) (throwError $ DuplicateDataConstructor con) ()

    -- are we currently declaring two constructors with the same name?
    whenM (return $ con `elem` (delete con constructors))
          (throwError $ DuplicateDataConstructor con)
          ()

    -- are any ADTs in this type fully applied?
    containsFullyAppliedTycons t

    {----------- The checks below this relate to ADTs vs GADTs -----------}

    -- does it only use the variables bound by the data declaration?
    let vars = tvars t
    forM_ vars $ \v ->
      whenM (return $ not $ v `elem` vars)
            (throwError $ UnboundADTVariable tycon vars con t v)
            ()

    -- does it construct something of the declared type? E.g constructors in the
    -- type `data Test a b where ...` may only construct values of type `Test a b`.
    let restype = construction t -- this just unwraps a function type
    let types   = map TVar vars
    whenM (return $ not $ restype == (TAdt tycon types))
          (throwError $ NonADTConstruction con (TAdt tycon types) restype)
          ()

  where
      tvars :: Type -> [Ident]
      tvars t = nub $ allvars t

      allvars :: Type -> [Ident]
      allvars t = case t of
        TVar a      -> [a]
        TTup ts     -> concat $ map tvars ts
        TAdt uid ts -> concat $ map tvars ts
        TLam t1 t2  -> tvars t1 ++ tvars t2
        _           -> []

checkProgram :: Program () -> TC (Subst, Program Type)
checkProgram p = do
  -- check that the declared ADTs are not GADTs and that they are
  -- well formed etc
  mapM_ checkDataDeclaration (datatypes p)

  -- extend the environment with the data constructors in scope and then
  -- type check all functions in the program
  let allfunctions = functions p ++ [main p]
  (sub, funs) <- withConstructors (datatypes p) $ checkFunctions allfunctions

  let main' = last funs
  let funs' = init funs
  return (sub, p { functions = funs'
               , main      = main'
               }
         )

checkFunctions :: [Function ()] -> TC (Subst, [Function Type])
checkFunctions fs = checkFunctions_ fs unitsub

checkFunctions_ :: [Function ()] -> Subst -> TC (Subst, [Function Type])
checkFunctions_ [] s     = return (s, [])
checkFunctions_ (f:fs) s = do
  (sub, f')   <- checkFunction f
  let id      = name f'
  let ty      = fromJust $ defVar $ head $ equations f'
  env         <- ask
  let schema  = generalize ty env
  (sub', fs') <- inEnv id schema $ checkFunctions_ fs (s `compose` sub)
  return $ (sub', f' : fs')

typecheck :: [Def ()] -> IO (Either String (Program Type, Subst))
typecheck defs = do
  let excepted = runExceptT $ checkProgram program
  let stated   = evalStateT excepted (TCState 0 Map.empty)
  readed       <- runReaderT stated initialEnv
  case readed of
    Left err -> return $ Left $ show err
    Right (subs,annotated) -> return (Right (annotated, subs))
  where
      program :: Program ()
      program = mkProgram defs

      funs :: [Function ()]
      funs = mkFunctions defs

      initialEnv :: Env
      initialEnv = Env (Map.fromList entries) Map.empty

      entries :: [(Ident, Schema)]
      entries = [ (Ident "add2", Forall [] $ TLam TInt TInt)
                , (Ident "prim_id", Forall [a] $ TLam ta ta)
                ]
        where
            a = Ident "a"
            ta = TVar a

instance {-# OVERLAPPING #-} Show ([Function Type], Subst) where
  show (funs, subst) = unlines [pfuns, psubs]
    where
       pfuns = unlines $ map printTree funs
       psubs = unlines $
                          map
                            (\(id,t) -> printTree id ++ " -> " ++ printTree t)
                            (Map.toList subst)

instance {-# OVERLAPPING #-} Print a => Show [Function a] where
  show funs = intercalate "\n\n" $ map printTree funs

runTC :: TC a -> IO (Either TCError a)
runTC tca = let excepted = runExceptT tca
                stated   = evalStateT excepted (TCState 0 Map.empty)
            in runReaderT stated (Env Map.empty Map.empty)

checkCaseClauses :: Type -> [(Pat (), Exp ())] -> TC (Subst, [(Pat Type, Exp Type)])
checkCaseClauses t []     = return (unitsub, [])
checkCaseClauses t (c:cs) = do
  (sub,c')   <- checkCaseClause t c
  (sub',cs') <- checkCaseClauses t cs
  return (sub `compose` sub', c':cs')

checkCaseClause :: Type -> (Pat (), Exp ()) -> TC (Subst, (Pat Type, Exp Type))
checkCaseClause ct (p,e) = do
  p'          <- checkPat p
  s1          <- unify ct (patVar p')
  let p''     = apply s1 p'
  let vars    = patBindings p''
  let schemas = map (\(id,t) -> (id, Forall [] t)) vars
  (s2,e')     <- inEnvMany schemas $ checkExp e
  return (s1 `compose` s2, (p'', e'))

checkExp :: Exp () -> TC (Subst, Exp Type)
checkExp e = case e of
  EVar () id      -> do
    t <- lookupVar id
    return (unitsub, EVar t id)

  ECon () uid     -> do
    t <- lookupCon uid
    return (unitsub, ECon t uid)

  ELit () l       -> return (unitsub, ELit (constType l) l)

  ECase () e pms  -> do
    (s1,e')      <- checkExp e
    (s2, pms')   <- checkCaseClauses (expVar e') pms
    s3           <- unifyAll $  map (expVar . snd) pms'
    let finsub   = s1 `compose` s2 `compose` s3
    let casetyps = expVar $ (snd . head) pms'
    return (finsub, ECase casetyps e' pms')

  ETup () es      -> do
    (subs, es') <- unzip <$> mapM checkExp es
    let typs = map expVar es'
    let sub = foldl1 compose subs
    return (sub, ETup (TTup typs) es')

  EBin () e1 e2 o -> do
    (s1, e1')      <- checkExp e1
    (s2, e2')      <- checkExp e2
    tv             <- fresh
    let inftype    = TLam (expVar e1') (TLam (expVar e2') tv)
    let candidates = binopCandidates o
    s3             <- unifyWithAtleastOne inftype candidates
    let sub        = s1 `compose` s2 `compose` s3
    return (sub, EBin (apply sub tv) e1' e2' (setBinopVar (apply sub inftype) o))

  EUn () e o      -> do
    (s1,e')        <- checkExp e
    tv             <- fresh
    let inftype    = TLam (expVar e') tv
    let candidates = unopCandidates o
    s2             <- unifyWithAtleastOne inftype candidates
    let sub        = s1 `compose` s2
    return (sub, EUn (apply sub tv) e' (setUnopVar (apply sub inftype) o))

  ELam () p e     -> do
    (p', vars)     <- checkPatAndBindings p
    let varschemas = map (\(id,t) -> (id, Forall [] t)) vars
    (sub,e')       <- inEnvMany varschemas $ checkExp e
    let lamtyp     = TLam (patVar p') (expVar e')
    return (sub, ELam lamtyp p' e')

  EApp () e1 e2   -> do
    tv          <- fresh
    (s1, e1')   <- checkExp e1
    (s2, e2')   <- local (apply s1) $ checkExp e2
    let t1      = expVar e1'
    let t2      = expVar e2'
    let inftyp  = TLam t2 tv
    s3          <- unify (apply s2 t1) inftyp
    let sub     = s3 `compose` s2 `compose` s1
    return (sub, EApp (apply sub tv) e1' e2')

  ELet () p e1 e2 -> do
      (s1, e1')      <- checkExp e1
      (p', vars)     <- checkPatAndBindings p
      s2             <- unify (patVar p') (expVar e1')
      env            <- ask
      let env'       = apply (s1 `compose` s2) env
      let varschemas = map (\(id,t) -> (id, generalize (apply (s1 `compose` s2) t) env')) vars
      let env''      = foldl (\e' (id,sc) -> extend (restrict e' id) id sc) env' varschemas
      (s3,e2')       <- local (const env'') $ checkExp e2
      return (s1 `compose` s2 `compose` s3, ELet (expVar e2') p' e1' e2')

  EIf () e1 e2 e3 -> do
    (s1,e1') <- checkExp e1
    (s2,e2') <- checkExp e2
    (s3,e3') <- checkExp e3
    s4 <- unify (expVar e1') TBool
    s5 <- unify (expVar e2') (expVar e3')
    let sub = s1 `compose` s2 `compose` s3 `compose` s4 `compose` s5
    return (sub, EIf (expVar e2') e1' e2' e3')

data PPState = ST 
     { source       :: T.Text  -- ^ The contents that remains unpreprocessed so far
     , current      :: Int     -- ^ The column number of the last fetched token
     , targetIndent :: [Int]}  -- ^ A stack of stored columns
  deriving Show

data Error = IndentationError Int Int String
instance Show Error where
    show (IndentationError expected found token) =
        "Parse error: expected indentation " ++ show expected ++ 
        " of token " ++ token ++ " but found " ++ show found

type PP a = StateT PPState (Writer T.Text) a

keywords :: [T.Text]
keywords = ["where", "of"]

-- | Function that will take a `Text` and preprocess it
process :: T.Text -> T.Text
process t =
    let wr = runStateT process_ (ST t 0 [0])
        ((_,_),w) = runWriter wr
        w' = T.dropWhile (/= ';') w
    in T.snoc (T.tail w') ';'

process_ :: PP ()
process_ = do
    (t,i) <- nextToken
    if t /= T.empty
        then (do
            while (getCurrentTarget >>= \i' -> if t `T.isPrefixOf` "--" ||
                                                  t `T.isPrefixOf` "{-" ||
                                                  t `T.isPrefixOf` "-}"
                                               then return False
                                               else emitFluff i i')
            tell t
            when (isKeyword t) (do
                tell "{"
                (t',i') <- nextToken
                pushCurrentTarget i'
                tell t')
            process_)
        else while (do
            target <- getCurrentTarget
            if target > 0
                then tell "}" >> popCurrentTarget >> return True
                else return False)

emitFluff :: Int -> Int -> PP Bool
emitFluff i1 i2
  | i1 < i2  = tell "}" >> popCurrentTarget >> return True
  | i1 == i2 = tell ";" >> return False
  | i1 > i2  = return False

getCurrentTarget :: PP Int
getCurrentTarget = head <$> gets targetIndent

popCurrentTarget :: PP ()
popCurrentTarget = modify $ \(ST r c (_:ts)) -> ST r c ts

pushCurrentTarget :: Int -> PP ()
pushCurrentTarget i = modify $ \(ST r c ts) -> ST r c (i:ts)

isKeyword :: T.Text -> Bool
isKeyword token = token `elem` keywords

nextToken :: PP (T.Text, Int)
nextToken = do
    (ST t i ti) <- get

    -- first split the input up in its leading whitespace/newline and the rest
    let (fluff, t')  = T.span (\c -> c == ' ' || c == '\n') t
    -- then extract the next token and the rest of the input
    let (token, t'') = T.span (\c -> c /= ' ' && c /= '\n') t'


    -- update the internal column counter
    updateColumn fluff
    c <- gets current

    -- emit the things we just cut off, to keep
    -- the annotated code as close to the source as possible
    tell fluff

    -- modify the internal state to reflect the rest of the input
    modify $ \(ST _ c i) -> ST t'' c i

    -- get the column of the current token and return the subsequent pair
    column <- gets current
    return (token, column)

updateColumn :: T.Text -> PP ()
updateColumn t = do
    -- is there a newline?
    -- if no symbol matches span will not 'consume' anything
    let (spaces, rest) = T.span (== '\n') t
    if spaces == T.empty
        -- if there wasn't, we count the number of spaces and add that to c
        then modify $ \(ST t' c i) -> ST t' (c + T.length rest) i
        -- otherwise we've encountered a newline and we set the internal counter
        -- to c and continue with a recursive call
        else modify (\(ST t' _ i)  -> ST t' 0 i) >> updateColumn rest

-- while mb returns true, execute ma
while :: Monad m => m Bool -> m ()
while mb = do
    b <- mb
    when b (while mb)

-- | Custom parser type - synonym for Parsec Void Text a
type Parser a = Parsec Void T.Text a

-- | Parser that parses a program
pProgram :: Parser [Def ()]
pProgram = many $ pDataDec <|> try pTypeSignature <|> pEquation

-- parse types
pClosed :: Parser Type
pClosed = choice [ TInt    <$ pSymbol "Int"
                 , TFloat  <$ pSymbol "Float"
                 , TBool   <$ pSymbol "Bool"
                 , TVar    <$> pIdent
                 , flip TAdt [] <$> pUIdent
                 , do pChar '('
                      ts <- sepBy pFun (pChar ',') <* pChar ')'
                      case ts of
                          []      -> pure TNil
                          [t]     -> pure t
                          (_:_:_) -> pure (TTup ts)
                 ]

pApp :: Parser Type
pApp = choice [ TAdt <$> pUIdent <*> many pClosed
              , pClosed
              ]

pFun :: Parser Type
pFun = foldr1 TLam <$> sepBy1 pApp (pSymbol "->")

pType :: Parser Type
pType = pSpace *> pFun

-- parse expressions

pExpClosed :: Parser (Exp ())
pExpClosed = choice [ ELit () <$> pConst
                    , EVar () <$> pIdent
                    , ECon () <$> pUIdent
                    , do pChar '('
                         es <- sepBy1 pExpVerbose (pChar ',') <* pChar ')'
                         case es of
                             []    -> undefined
                             [e]   -> pure e
                             (_:_:_) -> pure (ETup () es)
                    ]

pExpApp :: Parser (Exp ())
pExpApp = foldl1 (EApp ()) <$> some pExpClosed

pExpNot :: Parser (Exp ())
pExpNot = choice [ do pChar '!'
                      e <- pExpApp
                      pure $ EUn () e (Not ())
                 , pExpApp
                 ]

pExpMul :: Parser (Exp ())
pExpMul = pExpNot >>= go where
  go e1 = choice [ do pChar '*'
                      e2 <- pExpNot
                      go $ EBin () e1 e2 (Mul ())
                 , do pChar '/'
                      e2 <- pExpNot
                      go $ EBin () e1 e2 (Div ())
                 , pure e1
                 ]

pExpAdd :: Parser (Exp ())
pExpAdd = pExpMul >>= go where
  go e1 = choice [ do pChar '+'
                      e2 <- pExpMul
                      go $ EBin () e1 e2 (Add ())
                 , do pChar '-'
                      e2 <- pExpMul
                      go $ EBin () e1 e2 (Sub ())
                 , pure e1
                 ]

pExpRel :: Parser (Exp ())
pExpRel = pExpAdd >>= go where
    go e1 = choice [ do pChar '<'
                        choice [ do pChar '='
                                    e2 <- pExpAdd
                                    pure $ EBin () e1 e2 (OLE ())
                               , do e2 <- pExpAdd
                                    pure $ EBin () e1 e2 (OLT ())
                               ]
                   , do pChar '>'
                        choice [ do pChar '='
                                    e2 <- pExpAdd
                                    pure $ EBin () e1 e2 (OGE ())
                               , do e2 <- pExpAdd
                                    pure $ EBin () e1 e2 (OGT ())
                               ]
                   , do pSymbol "=="
                        e2 <- pExpAdd
                        pure $ EBin () e1 e2 (OEQ ())
                   , pure e1
                   ]

pExpAnd :: Parser (Exp ())
pExpAnd = foldr1 (\e1 e2 -> EBin () e1 e2 (And ())) <$> sepBy1 pExpRel (pSymbol "&&")

pExpOr :: Parser (Exp ())
pExpOr = foldr1 (\e1 e2 -> EBin () e1 e2 (Or ())) <$> sepBy1 pExpAnd (pSymbol "||")

pExpVerbose :: Parser (Exp ())
pExpVerbose = choice [
    do pSymbol "let"
       p <- pPat False False
       pSymbol "="
       e1 <- pExpVerbose
       pSymbol "in"
       ELet () p e1 <$> pExpVerbose
  , do pChar '\\'
       p <- pPat False False
       pSymbol "->"
       ELam () p <$> pExpVerbose
  , do pSymbol "if"
       e1 <- pExpVerbose
       pSymbol "then"
       e2 <- pExpVerbose
       pSymbol "else"
       e3 <- pExpVerbose
       return $ EIf () e1 e2 e3
  , do pSymbol "case"
       e <- pExpVerbose
       pSymbol "of"
       pChar '{'
       branches <- sepBy1 (do
         p <- pPat True True
         pSymbol "->"
         e <- pExpVerbose
         return (p,e)) (pChar ';')
       pChar '}'
       return $ ECase () e branches
  , pExpOr]

pExp :: Parser (Exp ())
pExp = pSpace *> pExpVerbose

  -- parse type signatures
pTypeSignature :: Parser (Def ())
pTypeSignature = do
    name <- pIdent
    pChar ':'
    t <- pType
    pChar ';'
    return $ DTypeSig name t

pDataDec :: Parser (Def ())
pDataDec = do
  pSymbol "data"
  uid <- pUIdent
  vars <- many pIdent
  pSymbol "where"
  pChar '{'
  constructors <- sepBy (do
    con <- pUIdent
    pChar ':'
    typ <- pType
    return (con,typ)) (pChar ';')
  pChar '}'
  pChar ';'
  return $ DDataDec uid vars constructors

  -- parse function clauses
pEquation :: Parser (Def ())
pEquation = do
    name <- pIdent
    patterns <- many (pPat True False)
    pSymbol "="
    exp <- pExp
    pChar ';'
    return $ DEquation () name patterns exp

-- parse patterns

pPatClosed :: Bool -> Bool -> Parser (Pat ())
pPatClosed allowConstants allowNary = choice $ maybe ++ always
  where maybe  = [PConst ()          <$> pConst | allowConstants]
        always = [ PVar  ()          <$> pIdent
                 , flip (PAdt ()) [] <$> pUIdent
                 , PWild ()          <$  pChar '_'
                 , do pChar '('
                      ps <- sepBy (pPatAs allowConstants allowNary) (pChar ',') <* pChar ')'
                      case ps of
                        []      -> pure $ PNil ()
                        [p]     -> pure p
                        (_:_:_) -> pure (PTup () ps)
                 ]

pPatApp :: Bool -> Bool -> Parser (Pat ())
pPatApp allowconstants allowNary = choice $ pAdt ++ [pPatClosed allowconstants allowNary]
  where
      pAdt = if allowNary
        then [adt]
        else [ try $ parens adt
             , pPatClosed allowconstants allowNary
             ]
      adt = do con  <- pUIdent
               vars <- many (pPatClosed allowconstants allowNary)
               return $ PAdt () con vars

pPatAs :: Bool -> Bool -> Parser (Pat ())
pPatAs allowConstants allowNary = choice
  [ try $ do x <- pIdent
             pSymbol "as"
             p <- pPatAs allowConstants allowNary
             return $ PAs () x p
  , pPatApp allowConstants allowNary]

pPat :: Bool -> Bool -> Parser (Pat ())
pPat allowConstants allowNary = pSpace *> pPatAs allowConstants allowNary

-- parse constants

pConst :: Parser Lit
pConst = choice [
    try $ LFloat  <$> Lexer.lexeme pSpace Lexer.float
  , LInt          <$> Lexer.lexeme pSpace Lexer.decimal
  , LBool         <$> ((True <$ pSymbol "True") <|> (False <$ pSymbol "False"))
  , LNil          <$  pSymbol "()"
  ]

-- parser utilities

parens :: Parser a -> Parser a
parens p = label "parse a type wrapped in parentheses" $ do
    pSymbol "("
    a <- p
    pSymbol ")"
    return a

pIdent :: Parser Ident
pIdent = try $ do
    a <- lowerChar
    rest <- many $ choice [letterChar, digitChar, char '_']
    trailings <- many (char '\'')
    pSpace
    let x = a:(rest++trailings)
    if x `elem` pkeywords
        then fail "found keyword, expected identifier"
        else return $ Ident x

pUIdent :: Parser UIdent
pUIdent = try $ do
    a <- upperChar
    rest <- many $ choice [letterChar, digitChar, char '_']
    pSpace
    let x = a:rest
    if x `elem` pkeywords
        then fail "found keyword, expected uppercase identifier"
        else pure $ UIdent x

pSymbol :: T.Text -> Parser T.Text
pSymbol = Lexer.symbol pSpace

pChar :: Char -> Parser ()
pChar c = void (char c <* pSpace)

pSpace :: Parser ()
pSpace = Lexer.space 
           (void spaceChar) 
           (Lexer.skipLineComment "--") 
           (Lexer.skipBlockComment "{-" "-}")

pkeywords :: [String]
pkeywords = [
  -- types
    "Bool"
  , "Int"
  , "Float"
  
  -- constants
  , "True"
  , "False"
  
  -- misc
  , "data"
  , "where"
  , "case"
  , "of"
  , "let"
  , "in"
  , "if"
  , "then"
  , "else"]

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print () where
  prt _ x = doc (shows x)

instance Print Int where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Bool where
  prt _ x = doc (shows x)

instance Print Ident where
  prt _ (Ident i) = doc $ showString i
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print UIdent where
  prt _ (UIdent i) = doc $ showString i

instance Print a => Print [Def a] where
  prt = prtList

instance Print a => Print (Def a) where
  prt i e = case e of
    DEquation a id pats exp -> prPrec i 0 (concatD [prt 0 id, prt 0 pats, doc (showString "="), prt 0 exp, doc (showString ":"), prt 0 a])
    DTypeSig id type_ -> prPrec i 0 (concatD [prt 0 id, doc (showString ":"), prt 0 type_])
    DDataDec uident ids constructordecs -> prPrec i 0 (concatD [doc (showString "data"), prt 0 uident, prt 0 ids, doc (showString "where"), doc (showString "{"), prt 0 constructordecs, doc (showString "}")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print (UIdent, Type) where
  prt i (uid,t) = prPrec i 0 (concatD [prt 0 uid, doc (showString ":"), prt 0 t])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [(UIdent, Type)] where
  prt = prtList

instance Print [Ident] where
  prt = prtList

instance Print Type where
  prt i e = case e of
    TLam type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])
    TVar id -> prPrec i 1 (concatD [prt 0 id])
    TTup ts -> prPrec i 1 (concatD ([doc (showString "(")] ++ printTups ts ++ [doc (showString ")")]))
    TInt -> prPrec i 2 (concatD [doc (showString "Int")])
    TFloat -> prPrec i 2 (concatD [doc (showString "Float")])
    TBool -> prPrec i 2 (concatD [doc (showString "Bool")])
    TNil -> prPrec i 2 (concatD [doc (showString "()")])
    TAdt uident types -> prPrec i 2 (concatD [prt 0 uident, prt 1 types])
    where
        printTups [] = []
        printTups [x] = [prt 0 x]
        printTups (x:y:xs) = [prt 0 x, doc (showString ",")] ++ printTups (y:xs) 
  prtList n [] = concatD []
  prtList n (x:xs) = concatD [prt n x, prt n xs]


instance Print [Type] where
  prt = prtList

instance Print a => Print (Exp a) where
  prt i e = case e of
    ELet a pat exp1 exp2 -> prPrec i 0 (concatD [doc (showString "let"), prt 0 pat, doc (showString "="), prt 0 exp1, doc (showString "in"), prt 0 exp2])
    ELam a pat exp -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 pat, doc (showString "->"), prt 0 exp])
    EApp a exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, prt 7 exp2])
    EBin a exp1 exp2 o -> prPrec i 4 (concatD [prt 4 exp1, prt 4 o, prt 5 exp2])
    EUn a e o -> prPrec i 4 (concatD [prt 4 o, prt 4 e])
    ETup a es -> prPrec i 7 (concatD ([doc (showString "(")] ++ printTups es ++ [doc (showString ")")]))
    EVar a id -> prPrec i 7 (concatD [prt 0 id])
    ECon a uid -> prPrec i 7 (concatD [prt 0 uid])
    ELit a const -> prPrec i 7 (concatD [prt 0 const])
    ECase a exp patmatchs -> prPrec i 0 (concatD [doc (showString "case"), prt 0 exp, doc (showString "of"), doc (showString "{"), prt 0 patmatchs, doc (showString "}")])
    EIf a exp1 exp2 exp3 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 exp1, doc (showString "then"), prt 0 exp2, doc (showString "else"), prt 0 exp3])
    where
        printTups [] = []
        printTups [x] = [prt 0 x]
        printTups (x:y:xs) = [prt 0 x, doc (showString ",")] ++ printTups (y:xs)

  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print a => Print (Pat a, Exp a) where
  prt i (pat,exp) = prPrec i 0 (concatD [prt 0 pat, doc (showString "->"), prt 0 exp])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print a => Print [Exp a] where
  prt = prtList

instance Print a => Print (Binop a) where
  prt i op = case op of
    Add a -> prPrec i 2 (concatD [doc (showString "+")])
    Sub a -> prPrec i 2 (concatD [doc (showString "-")])
    Mul a -> prPrec i 2 (concatD [doc (showString "*")])
    Div a -> prPrec i 2 (concatD [doc (showString "/")])
    OLT a -> prPrec i 2 (concatD [doc (showString "<")])
    OLE a -> prPrec i 2 (concatD [doc (showString "<=")])
    OGT a -> prPrec i 2 (concatD [doc (showString ">")])
    OGE a -> prPrec i 2 (concatD [doc (showString ">=")])
    OEQ a -> prPrec i 2 (concatD [doc (showString "==")])
    And a -> prPrec i 2 (concatD [doc (showString "&&")])
    Or a  -> prPrec i 2 (concatD [doc (showString "||")])

instance Print a => Print (Unop a) where
  prt i op = case op of
    Not a -> prPrec i 2 (concatD [doc (showString "!")])

instance Print Lit where
  prt i e = case e of
    LInt n -> prPrec i 0 (concatD [prt 0 n])
    LFloat n -> prPrec i 0 (concatD [prt 0 n])
    LBool n -> prPrec i 0 (concatD [prt 0 n])
    LNil -> prPrec i 0 (concatD [doc (showString "()")])

instance Print (Pat a) where
  prt i e = case e of
    PConst _ const -> prPrec i 0 (concatD [prt 0 const])
    PVar _ id -> prPrec i 0 (concatD [prt 0 id])
    PNil _ -> prPrec i 0 (concatD [doc (showString "()")])
    PWild _ -> prPrec i 0 (concatD [doc (showString "_")])
    PAs _ id pat -> prPrec i 2 (concatD [prt 0 id, doc (showString "as"), prt 0 pat])
    PAdt _ uident [] -> prPrec i 0 (concatD [prt 0 uident])
    PAdt _ uident adtpats -> prPrec i 0 (concatD [doc (showString "("), prt 0 uident, prt 0 adtpats, doc (showString ")")])
    PTup _ ps -> prPrec i 1 (concatD $ [doc (showString "(")] ++ printTups ps ++ [doc (showString ")")])
    where
        printTups [] = []
        printTups [x] = [prt 0 x]
        printTups (x:y:xs) = [prt 0 x, doc (showString ",")] ++ printTups (y:xs) 
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Pat a] where
  prt = prtList

instance Print a => Print (Function a) where
  prt i f = prtList 0 $ concat [maybe [] (\t -> [DTypeSig (name f) t]) (typesig f), equations f]
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print a => Print (Program a) where
  prt i p = concatD [prtList 0 datadecs, prtList 0 (functions p ++ [main p])]
    where
        datadecs :: [Def a]
        datadecs = map (\(uid, vars, cons) -> DDataDec uid vars cons) (datatypes p)

\end{code}