module CamIoT.Typecheck.Syntax where

import CamIoT.Internal.Syntax
import CamIoT.Util
import CamIoT.Typecheck.Environment
import CamIoT.Typecheck.Unification
import CamIoT.Typecheck.Substitution
import CamIoT.Typecheck.TCError

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State


-- * Generalizing and Instantiating types

{- | To generalize a type means to bind the free type variables and turn it into
a type schema. E.g in the environment @["f" ~> a]@,

@
> generalize $ a -> b
forall b . a -> b
@ -}
generalize :: Type -> Env -> Schema
generalize t env = Forall vars t
  where
      vars :: [Ident]
      vars = Set.toList $ ftv t `Set.difference` ftv env

{- | To instantiate a type schema means to replace the bound type variables with
freshly generated type variables. E.g

@
> instantiate $ forall b . a -> b
a -> c
@

where @c@ is a previously unseen type variable. -}
instantiate :: Schema -> TC Type
instantiate (Forall vars t) = do
  freshtypevars <- mapM (const fresh) vars
  let s = Map.fromList $ zip vars freshtypevars
  return $ apply s t

-- * Environment management

-- | Look up the type of an identifier
lookupVar :: Ident -> TC Type
lookupVar id = do
  (Env env _) <- ask
  case Map.lookup id env of
    Just schema -> instantiate schema
    Nothing     -> throwError $ UnboundVariable id

-- | Look up the type of a data constructor
lookupCon :: UIdent -> TC Type
lookupCon uid = do
  (Env _ env) <- ask
  case Map.lookup uid env of
    Just schema -> instantiate schema
    Nothing -> throwError $ UnknownConstructor uid

-- | Look up the type of a binary operator
lookupBinop :: Binop () -> TC Type
lookupBinop op =
  case Map.lookup op binoptypes of
    Just schema -> instantiate schema
    Nothing     -> throwError $ UnknownBinop op

-- * Typechecking helper-functions

{- | This function verifies that a type has all fully applied type constructors. If
it does not, an error is thrown. -}
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

{- | @moreGeneralThan t1 t2@ returns @True@ of the type @t1@ is more general than
the type @t2@. This function is used to ensure that the declared type of a function
is not more general than the inferred type of the function. If a function @f@ actually
has the type @Int -> Int@, it can not be declared to be of type @a -> a@. -}
moreGeneralThan :: Type -> Type -> Bool
moreGeneralThan (TVar _) (TVar _)     = False
moreGeneralThan (TVar _) _            = True
moreGeneralThan (TTup ts1) (TTup ts2) =
  or $ zipWith moreGeneralThan ts1 ts2
moreGeneralThan (TAdt _ ts1) (TAdt _ ts2) =
  or $ zipWith moreGeneralThan ts1 ts2
moreGeneralThan (TLam t1 t2) (TLam t1' t2') =
  moreGeneralThan t1 t1' || moreGeneralThan t2 t2'
moreGeneralThan _ _ = False

{- | Checks that the declared type of a function is not more general than the inferred
type. If that is the case a type error is raised. -}
checkTooGeneralType :: Ident -> Maybe Type -> Type -> TC ()
checkTooGeneralType _ Nothing _      = return ()
checkTooGeneralType fun (Just sig) t = do
  -- fetch the environment and generalize the type signature, and then instantiate it
  e <- ask
  let sch = generalize sig e
  sig' <- instantiate sch

  {- perform the actual check. If it is the case that the type signature is too general,
  the inferred is included in the raised error. Before this is done the type variables
  in the instantiated type signature are replaced with those in the declared type
  signature, to make it easier to read. -}
  if sig' `moreGeneralThan` t
    then do let inft = renameTVars sig t
            throwError $ TypeSignatureTooGeneral fun sig inft 
    else return ()
  where
    {- | Rename the type variables in the second argument with the corresponding
    type variables in the first type. -}
    renameTVars :: Type -> Type -> Type
    renameTVars t1 t2 = case (t1, t2) of
      (TVar id, TVar _)          -> TVar id
      (TTup ts1, TTup ts2)       -> TTup (zipWith renameTVars ts1 ts2)
      (TAdt uid ts1, TAdt _ ts2) -> TAdt uid $ zipWith renameTVars ts1 ts2
      (TLam t1 t2, TLam t1' t2') -> TLam (renameTVars t1 t1') (renameTVars t2 t2')
      (_,t)                      -> t

{- | Check that the declaration of a datatype is okay. Being okay means that it is not
only well formed, but that it is also an ordinary ADT, and not a GADT. If the check
fails, a type error is raised. -}
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
      -- | Returns all uniqye type variables in a type
      tvars :: Type -> [Ident]
      tvars t = nub $ allvars t

      -- | Returns all type variables in a type
      allvars :: Type -> [Ident]
      allvars t = case t of
        TVar a      -> [a]
        TTup ts     -> concat $ map tvars ts
        TAdt uid ts -> concat $ map tvars ts
        TLam t1 t2  -> tvars t1 ++ tvars t2
        _           -> []

{- | Make sure than an inferred type can unify the the declared type, if any type was
declared. Raises a type error if the check failed. -}
unifyWithTypesig :: Ident -> Maybe Type -> Type -> TC ()
unifyWithTypesig _ Nothing _      = return ()
unifyWithTypesig fun (Just sig) t = do
  catchError (unify sig t >> return ()) $ \_ ->
    throwError $ TypesigError fun sig t

-- * Typecheck constant literals

-- | `constType` returns the type of a literal
constType :: Lit -> Type
constType (LInt _)   = TInt
constType (LFloat _) = TFloat
constType (LBool _)  = TBool
constType LNil       = TNil

-- * Typecheck patterns

{- | Typecheck a pattern. The patterns are annotated with types, and any variables are
annotated with fresh type variables. -}
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
  PAdt () uid ps -> do
    -- look up the type of the constructor
    t            <- lookupCon uid
    -- typecheck the constructor arguments
    ps'          <- mapM checkPat ps

    -- fetch the types of the constructor arguments and construct a function
    -- type from the argument types to the constructor target
    let ptyps    = map patVar ps'
    let targ     = construction t
    let functype = funtype $ ptyps ++ [targ]

    -- unify the type of the constructor with the inferred types of the
    -- constructor arguments and the constructor target
    sub          <- unify t functype

    -- apply the substitution to the constructor arguments and the constructor
    -- target, and then return the annotated pattern
    let ps''     = apply sub ps'
    let targ'    = apply sub targ
    return $ PAdt targ' uid ps''
  PTup () ps -> do
    ps' <- mapM checkPat ps
    let tuptype = TTup $ map patVar ps'
    return $ PTup tuptype ps'

{- | Given a pattern, `patBindings` will return a list of variables and the types
they have. -}
patBindings :: Pat Type -> [(Ident, Type)]
patBindings p = case p of
  PVar t id     -> [(id,t)]
  PNil _        -> []
  PConst _ l    -> []
  PWild _       -> []
  PAs t id p    -> (id, t) : patBindings p
  PAdt _ uid ps -> concat $ map patBindings ps
  PTup _ ps     -> concat $ map patBindings ps

{- | Utility function which first typechecks a pattern and then also retrieves the
bindings of variables and types from the pattern. -}
checkPatAndBindings :: Pat () -> TC (Pat Type, [(Ident, Type)])
checkPatAndBindings p = do
  p' <- checkPat p
  let bindings = patBindings p'
  return (p', bindings)

-- * Typecheck expressions

-- | Typecheck an expression and return the annotated expression and a substitution
checkExp :: Exp () -> TC (Subst, Exp Type)
checkExp e = case e of
  EVar () id      -> do
    t <- lookupVar id
    env <- ask
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
      -- let p = e1 in e2

      -- typecheck e1 and acquire the substitution for it
      (s1, e1')      <- checkExp e1

      -- typecheck p and grab the variables it binds
      (p', vars)     <- checkPatAndBindings p

      -- unify the type of p with that of e1
      s2             <- unify (patVar p') (expVar e1')

      -- fetch the environment and apply the composition (s1 `compose` s2) on it
      env            <- ask
      let sub        = s1 `compose` s2
      let env'       = apply sub env

      --  generalize the types in the pattern using this substituted environment
      let varschemas = map (\(id,t) -> (id, Forall [] t)) vars --generalize (apply sub t) env')) vars
      let env''      = foldl (\e' (id,sc) -> extend (restrict e' id) id sc) env' varschemas

      -- after having extended the environment with the variables in pat, check e2
      (s3,e2')       <- local (const env'') $ checkExp e2

      -- return the annotated expression and the composition of all substitutions
      return (s3 `compose` s2 `compose` s1, ELet (expVar e2') p' e1' e2')

  EIf () e1 e2 e3 -> do
    tv <- fresh
    (sub, [e1',e2',e3'], t) <- inferPrim [e1,e2,e3] [TBool, tv, tv] tv
    return (sub, EIf t e1' e2' e3')

{- |  This code below is borrowed and modified from Stephen Diehl's repository. It takes
a list of expressions and a list of types, where the types and expressions must pairwise
unify after inferring the type of the expression. The third argument is the type of
the entire expression that was typechecked initially. Please see e.g how this is used
with `EIf` to understand what I mean by this. The result is a substitution, the list of
annotated expressions and the annotated type (the third argument). -}
inferPrim :: [Exp ()] -> [Type] -> Type -> TC (Subst, [Exp Type], Type)
inferPrim l ts typ = do
  env          <- ask
  (s, _, exps) <- foldM inferStep (unitsub, env, []) $ zip l ts
  return (s, reverse exps, apply s typ)
  where
    {- | Infers the type of the expression in the pair and unifies it with the type in
    the pair, by applying the input substitution to the input environment before
    inference. The result is the input substitution composed with the substitutitons
    produced during typechecking, the same environment and the list of expressions that
    was given as a parameter, but with the newly typechecked expression as the head. -}
    inferStep :: (Subst, Env, [Exp Type])
              -> (Exp (), Type)
              -> TC (Subst, Env, [Exp Type])
    inferStep (s, env, exps) (exp, typ) = do
      (s', t) <- local (const (apply s env)) $ checkExp exp
      s''     <- unify (expVar t) typ
      return (s'' `compose` s' `compose` s, env, t:exps)

{- | Typecheck the case clauses of a case-expression. @checkCaseClauses t clauses@
will return a substitution and the annotated clauses. @t@ is the type the expression
that was cased over has. The pattern in all case clauses must unify with this type. -}
checkCaseClauses :: Type -> [(Pat (), Exp ())] -> TC (Subst, [(Pat Type, Exp Type)])
checkCaseClauses t []     = return (unitsub, [])
checkCaseClauses t (c:cs) = do
  (sub,c')   <- checkCaseClause t c
  (sub',cs') <- checkCaseClauses t cs
  return (sub `compose` sub', c':cs')
  where
     checkCaseClause :: Type -> (Pat (), Exp ()) -> TC (Subst, (Pat Type, Exp Type))
     checkCaseClause ct (p,e) = do
       {- Typecheck the pattern and unify it with the case type. Apply the resulting
       substitution to the pattern before typechecking the expression. -}
       p'          <- checkPat p
       s1          <- unify ct (patVar p')
       let p''     = apply s1 p'
       let vars    = patBindings p''

       {- Extend the environment with the variables bound in the pattern before checking
       the expression. -}
       let schemas = map (\(id,t) -> (id, Forall [] t)) vars
       (s2,e')     <- inEnvMany schemas $ checkExp e
       return (s1 `compose` s2, (p'', e'))

-- * Typecheck definitions

-- | Typecheck a definition and return the annotated definition and a substitution
checkDef :: ([Pat ()], Exp ()) -> TC (Subst, ([Pat Type], Exp Type), Type)
checkDef (args, body) = do
  -- annotate arguments with type information
  args'           <- mapM checkPat args
  -- fetch the declared variables and their types from the arguments
  let argbindings = concat $ map patBindings args'
  -- convert them to schemas, to extend the environment with
  let argschemas  = map (\(id,t) -> (id, Forall [] t)) argbindings
  -- typecheck the equation body
  (sub, body')    <- inEnvMany argschemas $ checkExp body
  -- create the inferred type of the entire definition
  let functype    = apply sub $ foldr TLam (expVar body') $ map patVar args'
  return (sub, (args', body'), functype)

-- * Typecheck functions

-- | Typecheck a function and return the annotated function and a substitution
checkFunction :: Function () -> TC (Subst, Function Type)
checkFunction f = do
  -- typecheck equations and fetch their types
  subneqs <- case (typesig f) of
    Just t -> do
      env     <- ask
      let sch = generalize t env
      inEnv (name f) sch $ mapM checkDef $ equations f
    Nothing -> mapM checkDef $ equations f
  -- [(substitutions for each equation, the equation)]
  let subs = map (\(x,_,_) -> x) subneqs
  let eqs  = map (\(_,x,_) -> x) subneqs
  let types = map (\(_,_,x) -> x) subneqs

  -- create the mega-substitution for everything by unification
  let sub    = foldl compose unitsub subs
  sub'       <- unifyAll types
  let finsub = sub `compose` sub'

  -- apply the substitution to the equations and fetch the finished type
  -- of the entire function
  let eqs'        = apply finsub eqs
  let eqt         = apply finsub (head types)

  -- does the inferred type unify with the type signature, if any exist?
  unifyWithTypesig    (name f) (typesig f) eqt
  -- is the declared type not more general than the inferred one?
  checkTooGeneralType (name f) (typesig f) eqt

  let f'          = f { equations = eqs'
                      , typesig   = maybe (Just (apply finsub eqt)) Just (typesig f)
                      }

  -- return annotated, substituted function
  return $ (finsub, f')

-- | Typecheck functions and return the annotated functions and a substitution
checkFunctions :: [Function ()] -> TC (Subst, [Function Type])
checkFunctions fs = checkFunctions_ fs unitsub
  where
     checkFunctions_ :: [Function ()] -> Subst -> TC (Subst, [Function Type])
     checkFunctions_ [] s     = return (s, [])
     checkFunctions_ (f:fs) s = do
       (sub, f')   <- checkFunction f
       let id      = name f'
       let ty      = fromJust $ typesig f'
       env         <- ask
       let schema  = generalize ty env
       (sub', fs') <- inEnv id schema $ checkFunctions_ fs (s `compose` sub)
       return $ (sub', f' : fs')

-- * Typecheck programs

-- | Typecheck a program and return the annotated program and a substitution
checkProgram :: Program () -> TC (Subst, Program Type)
checkProgram p = do
  -- check that the declared ADTs are not GADTs and that they are
  -- well formed etc
  foldM (\acc d -> do withConstructors acc $ checkDataDeclaration d
                      return $ d : acc)
        []
        (datatypes p)

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
