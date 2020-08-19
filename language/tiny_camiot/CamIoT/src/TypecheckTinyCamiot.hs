{-# LANGUAGE FlexibleInstances #-}
module TypecheckTinyCamiot where

import AbsTinyCamiot
import PrintTinyCamiot

import Environment
import Unification
import AstUtils
import TCUtils

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Except
import Data.Maybe
import Data.List
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set

runTC :: TC [Def ()] -> TEnv -> IO (Either TCError Subst)
runTC tc initEnv = do
    let rd = runReaderT tc initEnv
    let wr = runWriterT rd
    let st = runStateT wr emptyState
    let ex = runExceptT st
    res <- ex
    case res of -- Either TCError (([Def ()]], [Constraint]), TCState)
        Left err -> return $ Left err
        Right ((annotatedTree, constraints), _) -> do
            --putStrLn $ intercalate "\n" $ map show constraints
            esubst <- runSolve constraints
            case esubst of
                (Left err')   -> return esubst
                (Right subst) -> do
                    putStrLn $ printTree $ apply subst annotatedTree
                    return (Right subst) 


{---------------------------------------------------------------------}
{- ******************** -}
-- top level typecheck

typecheck :: [Def ()] -> IO (Either TCError Subst)
typecheck defs = runTC (checkProgram defs) emptyEnv

-- collect type sigs
gatherTypeSigs :: [Def ()] -> TC [(Ident, Scheme)]
gatherTypeSigs ds = do
    let typesigs = catMaybes (map go ds)
    let funs     = map fst typesigs
    e           <- ask

    let typeschemes = map (\(n,t) -> (n, generalize e t)) typesigs

    case length funs == length (nub funs) of
        True  -> do
            modify (\e -> e { typesignatures = Map.fromList typeschemes})
            return $ map (\(n, t) -> (n, (generalize e t))) typesigs
        False -> throwError $ DuplicateTypeSig $ head (intersect funs (nub funs))

  where go (DTypeSig () fun t) = Just (fun, t)
        go _                   = Nothing

-- Gather type information about data declarations and their constructors
gatherDataDecs :: [Def ()] -> TC ()
gatherDataDecs [] = return ()
gatherDataDecs (d:ds) = case d of
    DDataDec () typ tvars cons -> do
        constructors <- mapM (consDecToType typ tvars) cons
        tryInsert constructors
        gatherDataDecs ds
    _ -> gatherDataDecs ds
  where
      tryInsert :: [(UIdent, Type ())] -> TC ()
      tryInsert []         = return ()
      tryInsert ((c,t):xs) = do
          env <- get
          e   <- ask
          let cons = constructors env
          case Map.lookup c cons of
              Just t' -> throwError $ DuplicateConstructor c t
              Nothing -> put (env { constructors = Map.insert c (generalize e t) cons}) >> tryInsert xs

      -- Given a UIdent, e.g 'Maybe', and a list of type vars, e,g [a], and
      -- a constructor, e.g Just, see if the type for Just is correct
      -- TODO check that the type variables are bound by the data declaration
      consDecToType :: UIdent -> [Ident] -> ConstructorDec () -> TC (UIdent, Type ())
      consDecToType typ tvars (ConstDec () con t) =
          -- get the intended creation
          let goal = getGoal t
          -- Is the intended creation an ADT?
          in case goal of
                                     -- Is it the correct ADT?
              (TAdt () con' vars) -> case typ == con' of
                           -- Is the arity correct?
                  True  -> case length tvars == length vars of
                               -- good!
                      True  -> let isOk = Set.isSubsetOf (ftv t) (Set.fromList tvars)
                                   diff = Set.difference (ftv t) (Set.fromList tvars)
                               in case isOk of
                                    True  -> return $ (con, t)
                                    False -> throwError $ UnboundTypeVariable tvars (Set.toList diff)
                      False -> throwError $ TypeArityError con' (map (TVar ()) tvars) vars
                  False -> throwError $ WrongConstructorGoal con goal (TAdt () typ (map (TVar ()) tvars))
              _ -> throwError $ WrongConstructorGoal con goal (TAdt () typ (map (TVar ()) tvars))
      
      getGoal (TLam _ _ r) = getGoal r
      getGoal t            = t

{- ******************** -}
-- typecheck

data Function = FN Ident (Maybe (Type ())) [Def ()]

checkProgram :: [Def ()] -> TC [Def ()]
checkProgram ds = do
    gatherDataDecs ds
    sigs <- gatherTypeSigs ds
    e <- ask
    let scope e = foldl (\e' (x, sc) -> extend e' (x, sc)) e sigs

    let (datadecls, functions) = makeFunctions ds
    ((++) datadecls . concat . map unwrapDef) <$> local scope (single functions)

  where single :: [Function] -> TC [Function]
        single [] = return []
        single (d:ds) = do
            (t, d') <- checkFunction d
            e <- ask
            let scope e = extend e (getName d, generalize e t)
            ds' <- local scope (single ds)
            return $ d' : ds'

        getName (FN n _ _) = n

        unwrapDef :: Function -> [Def ()]
        unwrapDef (FN _ Nothing ds) = ds
        unwrapDef (FN n (Just t) ds) = (DTypeSig () n t) : ds

-- groups function clauses together. Assumes definitions are given in the
-- proper order, i.d that they are not defined such as
-- foldl _ b [] = b
-- sum = foldl (+) 0
-- foldl ....
makeFunctions :: [Def ()] -> ([Def ()], [Function])
makeFunctions ds =
    let f (DEquation _ n1 _ _) (DEquation _ n2 _ _) = n1 == n2
        f (DTypeSig _ n1 _) (DEquation _ n2 _ _)    = n1 == n2
        f _ _                                       = False

        groups = groupBy f ds

        isDataDec [(DDataDec _ _ _ _)] = True
        isDataDec _                    = False

        (datadecs, funs) = partition isDataDec groups
    in (concat datadecs, makeFunctions_ funs)

makeFunctions_ :: [[Def ()]] -> [Function]
makeFunctions_ = map makeFun
  where makeFun ((DTypeSig _ name t):ds)      = FN name (Just t) ds
        makeFun ds@((DEquation _ name _ _):_) = FN name Nothing ds

-- given an annotated definition, return its type
typeOfAnnotatedDef :: Def () -> TC (Type ())
typeOfAnnotatedDef (DEquation _ _ pats exp) = do
    --e <- ask
    return $ {-generalize e -}(function_type (map getPatType pats) (getExpType exp))
typeOfAnnotatedDef _ = error "shouldn't end up here" -- TODO backtrack and solve

-- is a function recursive? We deduce that it is so if any of the
-- clause bodies uses the identifier bound in the definition.
recursive :: Function -> Bool
recursive (FN name _ bodies) = any recursiveSingle bodies
  where recursiveSingle (DEquation _ n _ e) = usesVar n e

hasTypeSig :: Function -> Bool
hasTypeSig (FN _ (Just _) _) = True
hasTypeSig _                 = False

checkFunction :: Function -> TC (Type (), Function)
checkFunction fun@(FN name sig clauses) = do
    clauses' <- case recursive fun of
        True  -> case hasTypeSig fun of
            True  -> mapM checkClause clauses
            False -> throwError $ RecursiveFunctionWithoutTypesig name
        False -> mapM checkClause clauses
    t <- unifyClauses clauses'
    case sig of
        (Just typ) -> do
            e <- ask
            let gentype = generalize e typ
            instantiated <- instantiate gentype

            let test _ t2 = case instantiated `isMoreGeneral` t2 of
                                Just True -> Just $ TypeSignatureTooGeneral name typ t2
                                _         -> Nothing
            uni instantiated t (Just test)
            return (typ, FN name sig clauses')
        Nothing    -> return (t, FN name sig clauses')

checkClause :: Def () -> TC (Def ())
checkClause (DEquation () name pats exp) = do
    -- get the types and variables bound in the declaration
    patinfo <- mapM (flip checkPattern True) pats
    let pats' = map fst patinfo
    let types = map getPatType pats'
    let vars  = concat $ map snd patinfo
    -- extend the environment with the variables
    exp' <- inEnvMany (map (\(x,t') -> (x, Forall [] t')) vars) (checkExp exp)
    -- get the result type and construct the annotated definition & the complete type
    return $ DEquation () name pats' exp'

-- given a function (the annotated clauses belonging to a single function), try to unify
-- them all and make sure that they are of the same type. Return the unified type.
unifyClauses :: [Def ()] -> TC (Type ())
unifyClauses ds = do
    types <- mapM typeOfAnnotatedDef ds
    let test t1 t2 = if t1 /= t2
                     then Just $ FunctionClausesNotEqual (name ds) t1 t2
                     else Nothing
    uniMany types (Just test)
    return $ last types
  where name ((DEquation _ n _ _):_) = n

-- If the types are of the same shape, returns true if the first operand
-- is more general than the second operand. If where there is a variable
-- in the first operand there is something else in the second, the left one
-- is more general.
--
-- If they are of different shapes there is something else wrong, and we should
-- just let the unifier figure out what it is?
isMoreGeneral :: Type () -> Type () -> Maybe Bool
isMoreGeneral (TLam _ t1 t1s) (TLam _ t2 t2s)     = (||) <$> 
                                                      (isMoreGeneral t1 t2) <*> 
                                                      (isMoreGeneral t1s t2s)
isMoreGeneral (TVar _ _) (TVar _ _)               = Just False
isMoreGeneral (TVar _ _) _                        = Just True
isMoreGeneral (TAdt _ con1 t1s) (TAdt _ con2 t2s) =
    -- there is surely something built in for this
    let maybes = zipWith isMoreGeneral t1s t2s
        f (Just x) (Just y) = Just (x || y)
        f Nothing  _        = Nothing
        f _        Nothing  = Nothing
    in foldl f (Just False) maybes
isMoreGeneral (TTup _ t1s) (TTup _ t2s)           = 
    let t1s' = map deTupType t1s
        t2s' = map deTupType t2s
        maybes = zipWith isMoreGeneral t1s' t2s'
        f (Just x) (Just y) = Just (x || y)
        f Nothing  _        = Nothing
        f _        Nothing  = Nothing
    in foldl f (Just False) maybes
isMoreGeneral _ _                                 = Nothing

-- Input: a pattern
-- output
--   - component 1: Type of the top-level pattern
--   - component 2: List of variables and their type variables created by the pattern
-- e.g pattern Just (a,2) gives us (Maybe (a, Int), [a, Int])
checkPattern :: Pat () -> Bool -> TC (Pat (), [(Ident, Type ())])
checkPattern p allowConstants = case p of
    -- Are constaints allowed? In e.g lambda abstractions we don't
    -- allow constants such as 3
    PConst a c       -> if allowConstants 
                        then return $ (PTyped () p (checkConstType c), []) 
                        else throwError $ LambdaConstError c

    -- variables are easy! Just generate a fresh type variable for it and return that
    PVar a var       -> fresh >>= \tv -> return (PTyped () p tv, [(var, tv)])

    -- 0-ary ADT constructor, just look up the type. No local variables are introduced
    PZAdt a con -> do
        t <- lookupCons (Constructor () con)
        return (PTyped () p t, [])

    -- N-ary ADT constructor
    PNAdt a con pats  -> do
        let unwrapped_pats = map deAdtPat pats
        res <- mapM (flip checkPattern allowConstants) unwrapped_pats

        let pats' = map fst res
        let typs  = map getPatType pats'
        let vars  = concat $ map snd res
        t <- lookupCons (Constructor () con)
        let numargs = count_arguments t

        -- Is it fully applied? We only pattern match on fully applied constructors.
        case length pats == numargs of
            True  -> do
                -- generate fresh type variable
                tv <- fresh
                -- unify the constructors type with the inferred type (patterns -> tv)
                uni t (function_type typs tv) Nothing
                return $ (PTyped () (PNAdt a con (map adtPat pats')) tv, vars)
            -- otherwise we are not fully applied, and we raise an error.
            False -> throwError $ ConstructorNotFullyApplied con numargs (length pats)

    -- wildcards can have any type they like?
    PWild a          -> fresh >>= \tv -> return (PTyped () p tv, [])

    PNil a           -> return (PTyped () p (TNil ()), [])

    -- The type of a pattern such as (3,5) is (Int, Int). Recursively check what type the
    -- patterns 3 and 5 have, and use those results to build a TTup node.
    PTup a patterns -> do
        let unwrapped_patterns = map deTupPat patterns
        res <- mapM (flip checkPattern allowConstants) unwrapped_patterns

        let patterns'  = map fst res
        let pat_types  = map getPatType patterns'
        let vars       = concat $ map snd res
        let patterns'' = map tupPat patterns'
        return $ (PTyped () (PTup a patterns'') (TTup () (map tupType pat_types)), vars)

        --res <- mapM (flip checkPattern allowConstants) (map deTupPat patterns)
        --let types = map fst res
        --let vars  = concat $ map snd res
        --return $ (TTup () (map tupType types), vars)
    
    PLay a var pat   -> do
        -- recursively check pattern
        (pat', vars) <- checkPattern pat allowConstants
        -- get the type of the pattern
        let tv = getPatType pat'
        -- return the annotated pattern and the new environment variables
        return $ (PTyped () (PLay a var pat') tv, (var, tv) : vars)

checkConstType :: Const () -> Type ()
checkConstType const = case const of
    CInt a integer  -> int
    CFloat a double -> float
    CTrue a         -> bool
    CFalse a        -> bool
    CNil a          -> TNil ()

-- Check many case branches. They must be of the same type.
-- E.g we do not allow
-- case x of
--   (Just a) -> 3
--   Nothing  -> True
-- as that is just silly
checkCases :: Type () -> [PatMatch ()] -> TC [PatMatch ()]
checkCases t pm = do
    pm' <- mapM (checkCase t) pm
    let types = map getPMType pm'
    uniMany types Nothing
    return $ pm'

-- Check that a case branch is correctly typed.
-- check the pattern and extend the environment with any
-- variables it might create before we check that the result is well typed.
-- We also verify that the pattern is of the correct type, which is the type
-- given as an argument.
checkCase :: Type () -> PatMatch () -> TC (PatMatch ())
checkCase t (PM () pat e1) = do
    (pat', vars) <- checkPattern pat True
    let t' = getPatType pat'
    uni t t' Nothing
    e1' <- inEnvMany (map (\(x, t'') -> (x, Forall [] t'')) vars) (checkExp e1)
    return $ PM () pat' e1'

-- Type check an expression!
checkExp :: Exp () -> TC (Exp ())
checkExp e = case e of
    ECase _ e1 patterns -> do
        -- check the type of the expression we are casing over
        e1' <- checkExp e1
        let te1 = getExpType e1'

        patterns' <- checkCases te1 patterns
                                                     -- TODO backtrack and see why it has to be reversed here
        let t = getPMType (head (reverse patterns')) -- parser only parses nonempty lists
        return $ ETyped () (ECase () e1' patterns') t

    ELet _ p e1 e2 -> do
        -- Check the type of the pattern
        (p', vars) <- checkPattern p False
        let tpat = getPatType p'
        -- check the type of the expression we are binding to the pattern
        e1' <- checkExp e1
        let te1 = getExpType e1'
        -- unify them! e.g let (a,b) = Nothing in ... makes no sense
        uni tpat te1 Nothing
        -- extend the environment with the new variable(s) and check e2
        e2' <- inEnvMany (map (\(x,t') -> (x, Forall [] t')) vars) (checkExp e2)
        let te2 = getExpType e2'
        return $ ETyped () (ELet () p' e1' e2') te2

    -- TODO
    ELetR _ pattern e1 e2 -> undefined

    ELam _ pattern e1 -> do
        -- check the pattern! We do not allow constants here.
        (pattern', vars) <- checkPattern pattern False
        let tpat = getPatType pattern'
        -- extend the environment with the variables bound by the lambda and
        -- check the function body.
        e1' <- inEnvMany (map (\(x,t') -> (x, Forall [] t')) vars) (checkExp e1)
        let t = getExpType e1'
        -- The whole expressions is of the type
        -- (type of pattern) -> (type of expression)
        return $ ETyped () (ELam () pattern' e1') (tpat *-> t)

    EIf _ e1 e2 e3 -> do
        -- check all three expression
        e1' <- checkExp e1
        e2' <- checkExp e2
        e3' <- checkExp e3
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        let te3 = getExpType e3'
        -- unify the first one with bool
        uni te1 bool Nothing
        -- unify the other two, both branches must have the same type
        uni te2 te3 Nothing
        -- return te2 (or te3, doesn't matter)
        return $ ETyped () (EIf () e1' e2' e3') te2

    EApp a e1 e2 -> do
        -- check the type of the (hopefully) function we are applying
        e1' <- checkExp e1
        let te1 = getExpType e1'
        -- check the type of the argument to the function
        e2' <- checkExp e2
        let te2 = getExpType e2'
        -- generate a fresh type variable for the 'result'
        tv <- fresh
        -- see if it is possible to unify the types. Essentially
        -- checking if the first argument of te1 is te2.
        uni te1 (te2 *-> tv) Nothing
        return $ ETyped () (EApp a e1' e2') tv

    EOr _ e1 e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        -- Are both expressions booleans? Then we are OK!
        uni te1 bool Nothing
        uni te2 bool Nothing
        return $ ETyped () (EOr () e1' e2') bool

    EAnd _ e1 e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        uni te1 bool Nothing
        uni te2 bool Nothing
        return $ ETyped () (EAnd () e1' e2') bool

    ERel _ e1 op e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        tv <- fresh
        -- u2 will become e.g int -> int -> int for (+)
        -- is it possible to unify te1 -> te2 -> tv and
        --                         int -> int -> int?
        let u1  = (te1 *-> te2 *-> tv)
            u2  = relOps Map.! op
            op' = RelOpTyped () op u2
        uni u1 u2 Nothing
        return $ ETyped () (ERel () e1' op' e2') tv

    EAdd _ e1 op e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        tv <- fresh
        let u1  = (te1 *-> te2 *-> tv)
            u2  = addOps Map.! op
            op' = AddOpTyped () op u2
        uni u1 u2 Nothing
        return $ ETyped () (EAdd () e1' op' e2') tv

    EMul _ e1 op e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        tv <- fresh
        let u1 = (te1 *-> te2 *-> tv)
            u2 = mulOps Map.! op
            op' = MulOpTyped () op u2
        uni u1 u2 Nothing
        return $ ETyped () (EMul () e1' op' e2') tv

    ENot _ e1 -> do
        e1' <- checkExp e1
        let te1 = getExpType e1'
        -- We can only negate booleans :)
        uni te1 bool Nothing
        return $ ETyped () (ENot () e1') bool

    -- Capital letter variables are constructors! Look up the type of it.
    EUVar _ con -> do
        t <- lookupCons (Constructor () con)
        return $ ETyped () (EUVar () con) t
    -- otherwise it is a function or something else, like an x bound by a \x.
    EVar _ var  -> do
        t <- lookupVar var
        return $ ETyped () (EVar () var) t

    ETup _ tupExps -> do
        let exps = map deTupExp tupExps
        -- check the types of the tuple expressions
        exps' <- mapM checkExp exps
        let texps = map getExpType exps'
        -- simply build a tuple type of the inferred types. Nothing to unify, tuples
        -- are polymorphic in their contents.
        let tupExps' = map tupExp exps'
        return $ ETyped () (ETup () tupExps') (TTup () (map tupType texps))

    EConst _ const -> case const of
        CInt a i   -> return $ ETyped () (EConst () const) int
        CFloat a f -> return $ ETyped () (EConst () const) float
        CTrue a    -> return $ ETyped () (EConst () const) bool
        CFalse a   -> return $ ETyped () (EConst () const) bool
        CNil a     -> return $ ETyped () (EConst () const) (TNil ())

-- TODO we can remove the 'duplice' operators and try to overload them
-- when we know how to attempt the unification when the types it can be
-- unified with are not arbitrary, but actually a subset of all types.
-- (I have an idea of how to do this, but let's do this when everything
--  else is complete)
addOps :: Map.Map (AddOp ()) (Type ())
addOps = Map.fromList [
    (Plus   (), TLam () int   (TLam () int   int)),
    (FPlus  (), TLam () float (TLam () float float)),
    (Minus  (), TLam () int   (TLam () int   int)),
    (FMinus (), TLam () float (TLam () float float))]

mulOps :: Map.Map (MulOp ()) (Type ())
mulOps = Map.fromList [
    (Times  (), TLam () int   (TLam () int   int)),
    (FTImes (), TLam () float (TLam () float float)),
    (Div    (), TLam () int   (TLam () int   int)),
    (FDiv   (), TLam () float (TLam () float float))]

relOps :: Map.Map (RelOp ()) (Type ())
relOps = Map.fromList [
    (LTC  (), TLam () int   (TLam () int   bool)),
    (FLTC (), TLam () float (TLam () float bool)),
    (LEC  (), TLam () int   (TLam () int   bool)),
    (FLEC (), TLam () float (TLam () float bool)),
    (GTC  (), TLam () int   (TLam () int   bool)),
    (FGTC (), TLam () float (TLam () float bool)),
    (GEC  (), TLam () int   (TLam () int   bool)),
    (FGEC (), TLam () float (TLam () float bool)),
    (EQC  (), undefined {- TODO insert some type later, talk with Joel -})]
    -- ok I know what to do now! The elements of the lists should be schemas, and
    -- when we fetch a type based on an operator we return the instantiated type.
