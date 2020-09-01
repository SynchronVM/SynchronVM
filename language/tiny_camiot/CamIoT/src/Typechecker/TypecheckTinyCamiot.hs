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
module Typechecker.TypecheckTinyCamiot where

import Parser.AbsTinyCamiot
import Parser.PrintTinyCamiot

import Typechecker.Environment
import Typechecker.Unification
import Typechecker.AstUtils
import Typechecker.TCUtils

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
                    --putStrLn $ showSubst subst
                    putStrLn $ printTree $ apply subst annotatedTree
                    return (Right subst) 

showSubst :: Map.Map Ident (Type ()) -> String
showSubst = intercalate "\n" . map (\(k,v) -> "binding " ++ printTree k ++ " to " ++ printTree v) . Map.toList

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
instance Show Function where
  show(FN name sig clauses) =
      "Function: " ++ printTree name ++ "\n" ++
      "Type signature: " ++ show (fmap printTree sig) ++ "\n" ++
      "Definitions:\n" ++ intercalate "\n" (map printTree clauses)

checkProgram :: [Def ()] -> TC [Def ()]
checkProgram ds = do
    gatherDataDecs ds
    sigs <- gatherTypeSigs ds
    e <- ask
    let scope e = foldl (\e' (x, sc) -> extend e' (x, sc)) e sigs

    let (datadecls, functions) = makeFunctions ds
    ((++) datadecls . concat . map unwrapDef) <$> local scope (single functions)

     -- typechecks the functions one by one, extending the environment
     -- with the previously typechecked functions types.
  where single :: [Function] -> TC [Function]
        single [] = return []
        single (d:ds) = do
            (t, d') <- checkFunction d
            e <- ask
            let scope e = extend e (getName d', generalize e t)
            ds' <- local scope (single ds)
            return $ d' : ds'

        getName (FN n _ _) = n

        unwrapDef :: Function -> [Def ()]
        unwrapDef (FN _ Nothing ds)  = ds
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
    return $ (function_type (map getPatType pats) (getExpType exp))
-- we only call this after we've already checked for single type definitions
typeOfAnnotatedDef _ = error "shouldn't end up here"

-- is a function recursive? We deduce that it is so if any of the
-- clause bodies uses the identifier bound in the definition.
recursive :: Function -> Bool
recursive (FN name _ bodies) = any recursiveSingle bodies
  where recursiveSingle (DEquation _ n _ e) = usesVar n e

hasTypeSig :: Function -> Bool
hasTypeSig (FN _ (Just _) _) = True
hasTypeSig _                 = False

checkFunction :: Function -> TC (Type (), Function)
checkFunction (FN name (Just t) []) = throwError $ AloneTypeSignature name t
checkFunction fun@(FN name sig clauses) = do
    clauses' <- case recursive fun of
        True  -> case hasTypeSig fun of
            True  -> mapM checkClause clauses
            False -> throwError $ RecursiveFunctionWithoutTypesig name
        False -> mapM checkClause clauses
    
    let fun' = FN name sig clauses'
    t <- unifyClauses fun'
    return $ (t, fun')

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

unifyClauses :: Function -> TC (Type ())
unifyClauses (FN name sig ds) = do
    let noSigTest t1 t2 = if t1 /= t2
                          then Just $ FunctionClausesNotEqual name t1 t2
                          else Nothing

    let okSigTest instantiated t1 t2 = case instantiated `isMoreGeneral` t2 of
            (Just True)  -> Just $ TypeSignatureTooGeneral name (fromJust sig) t2
            (Just False) -> Nothing
            Nothing      -> case t1 /= t2 of
                True  -> Just $ FunctionClauseWrongType name instantiated t2
                False -> Nothing

    case sig of
        Just typ -> do
            mapM (\d -> do
                e <- ask
                let gentype = generalize e typ
                instantiated <- instantiate gentype
                t' <- typeOfAnnotatedDef d
                uni instantiated t' (Just (okSigTest instantiated))) ds
            return typ
        Nothing  -> do
            types <- mapM typeOfAnnotatedDef ds
            uniMany types (Just noSigTest)
            return (last types)

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
        res <- mapM (flip checkPattern allowConstants) pats

        let pats' = map fst res
        let typs  = map getPatType pats'
        let vars  = concat $ map snd res
        t <- lookupCons (Constructor () con)
        let numargs = count_arguments t

        let test t1 t2 = if t1 /= t2
                         then Just $ PatternTypeError p t1 t2
                         else Nothing

        -- Is it fully applied? We only pattern match on fully applied constructors.
        case length pats == numargs of
            True  -> do
                -- generate fresh type variable
                tv <- fresh
                -- unify the constructors type with the inferred type (patterns -> tv)
                uni t (function_type typs tv) (Just test) -- Nothing
                return $ (PTyped () (PNAdt a con pats') tv, vars)
            -- otherwise we are not fully applied, and we raise an error.
            False -> throwError $ ConstructorNotFullyApplied con numargs (length pats)

    -- wildcards can have any type they like?
    PWild a          -> fresh >>= \tv -> return (PTyped () p tv, [])

    PNil a           -> return (PTyped () p (TNil ()), [])

    -- The type of a pattern such as (3,5) is (Int, Int). Recursively check what type the
    -- patterns 3 and 5 have, and use those results to build a TTup node.
    PTup a patterns -> do
        res <- mapM (flip checkPattern allowConstants) patterns

        let patterns'  = map fst res
        let pat_types  = map getPatType patterns'
        let vars       = concat $ map snd res
        return $ (PTyped () (PTup a patterns') (TTup () pat_types), vars)

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
        let u1  = (te1 *-> te2 *-> tv)
        
        u2 <- relOps op
        let op' = RelOpTyped () op u2

        uni u1 u2 Nothing
        case op of
            EQC _ -> uniEither [(u1, int *-> int *-> tv),
                                (u1, float *-> float *-> tv),
                                (u1, bool *-> bool *-> tv)]
            otherwise -> uniEither [(u1, int *-> int *-> tv),
                                    (u1, float *-> float *-> tv)]
        return $ ETyped () (ERel () e1' op' e2') tv

    EAdd _ e1 op e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        tv <- fresh
        let u1  = (te1 *-> te2 *-> tv)
        -- u1 = inferred type

        u2 <- addOps op
        -- u2 = stored type
        let op' = AddOpTyped () op u2
        
        uni u1 u2 Nothing
        uniEither [(u1, int *-> int *-> int),
                   (u1, float *-> float *-> float)]
        return $ ETyped () (EAdd () e1' op' e2') tv

    EMul _ e1 op e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        tv <- fresh
        let u1 = (te1 *-> te2 *-> tv)

        u2 <- mulOps op
        let op' = MulOpTyped () op u2

        uni u1 u2 Nothing
        uniEither [(u1, int *-> int *-> int),
                   (u1, float *-> float *-> float)]
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

    ETup _ exps -> do
        -- check the types of the tuple expressions
        exps' <- mapM checkExp exps
        let texps = map getExpType exps'
        return $ ETyped () (ETup () exps') (TTup () texps)

    EConst _ const -> case const of
        CInt a i   -> return $ ETyped () (EConst () const) int
        CFloat a f -> return $ ETyped () (EConst () const) float
        CTrue a    -> return $ ETyped () (EConst () const) bool
        CFalse a   -> return $ ETyped () (EConst () const) bool
        CNil a     -> return $ ETyped () (EConst () const) (TNil ())

addOps :: AddOp () -> TC (Type ())
addOps op = case Map.lookup op addOps_ of
    Just t  -> instantiate t
    Nothing -> error "see below"

addOps_ :: Map.Map (AddOp ()) Scheme
addOps_ = Map.fromList [
    (Plus   (), let id = Ident "a"
                    a  = TVar () id
                in Forall [id] (a *-> a *-> a)),
    (Minus  (), let id = Ident "a"
                    a  = TVar () id
                in Forall [id] (a *-> a *-> a))]

mulOps :: MulOp () -> TC (Type ())
mulOps op = case Map.lookup op mulOps_ of
    Just t  -> instantiate t
    Nothing -> error "see below"    

mulOps_ :: Map.Map (MulOp ()) Scheme
mulOps_ = Map.fromList [
    (Times  (), let id = Ident "a"
                    a = TVar () id
                in Forall [id] (a *-> a *-> a)),
    (Div    (), let id = Ident "a"
                    a = TVar () id
                in Forall [id] (a *-> a *-> a))]

relOps :: RelOp () -> TC (Type ())
relOps op = case Map.lookup op relOps_ of
    Just t  -> instantiate t
    Nothing -> error "we should not end up here, how can I enforce this?"

relOps_ :: Map.Map (RelOp ()) Scheme
relOps_ = Map.fromList [
    (LTC  (), let id = Ident "a"
                  a  = TVar () id
              in Forall [id] (a *-> a *-> bool)),
    (LEC  (), let id = Ident "a"
                  a  = TVar () id
              in Forall [id] (a *-> a *-> bool)),
    (GTC  (), let id = Ident "a"
                  a  = TVar () id
              in Forall [id] (a *-> a *-> bool)),
    (GEC  (), let id = Ident "a"
                  a  = TVar () id
              in Forall [id] (a *-> a *-> bool)),
    (EQC  (), let id = Ident "a"
                  a  = TVar () id
              in Forall [id] (a *-> a *-> bool))]