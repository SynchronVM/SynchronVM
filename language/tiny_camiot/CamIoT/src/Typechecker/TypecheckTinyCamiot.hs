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

runTC :: TC [Def Type] -> TEnv -> IO (Either TCError Subst)
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

showSubst :: Map.Map Ident Type -> String
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

  where go (DTypeSig fun t) = Just (fun, t)
        go _                   = Nothing

-- Gather type information about data declarations and their constructors
gatherDataDecs :: [Def ()] -> TC ()
gatherDataDecs [] = return ()
gatherDataDecs (d:ds) = case d of
    DDataDec typ tvars cons -> do
        constructors <- mapM (consDecToType typ tvars) cons
        tryInsert constructors
        gatherDataDecs ds
    _ -> gatherDataDecs ds
  where
      tryInsert :: [(UIdent, Type)] -> TC ()
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
      consDecToType :: UIdent -> [Ident] -> ConstructorDec -> TC (UIdent, Type)
      consDecToType typ tvars (ConstDec con t) =
          -- get the intended creation
          let goal = getGoal t
          -- Is the intended creation an ADT?
          in case goal of
                                     -- Is it the correct ADT?
              (TAdt con' vars) -> case typ == con' of
                           -- Is the arity correct?
                  True  -> case length tvars == length vars of
                               -- good!
                      True  -> let isOk = Set.isSubsetOf (ftv t) (Set.fromList tvars)
                                   diff = Set.difference (ftv t) (Set.fromList tvars)
                               in case isOk of
                                    True  -> return $ (con, t)
                                    False -> throwError $ UnboundTypeVariable tvars (Set.toList diff)
                      False -> throwError $ TypeArityError con' (map TVar tvars) vars
                  False -> throwError $ WrongConstructorGoal con goal (TAdt typ (map TVar tvars))
              _ -> throwError $ WrongConstructorGoal con goal (TAdt typ (map TVar tvars))
      
      getGoal (TLam  _ r) = getGoal r
      getGoal t            = t

{- ******************** -}
-- typecheck

data Function a = FN Ident (Maybe Type) [Def a]
instance Show a => Show (Function a) where
  show(FN name sig clauses) =
      "Function: " ++ printTree name ++ "\n" ++
      "Type signature: " ++ show (fmap printTree sig) ++ "\n" ++
      "Definitions:\n" ++ intercalate "\n" (map printTree clauses)

-- changing phantom type for definitions
fakeCoerce :: Def a -> Def Type
fakeCoerce (DDataDec tyvar vars cons) = DDataDec tyvar vars cons
fakeCoerce (DTypeSig name sig) = DTypeSig name sig
fakeCoerce otherwise = error "should not be invoked"

checkProgram :: [Def ()] -> TC [Def Type]
checkProgram ds = do
    gatherDataDecs ds
    sigs <- gatherTypeSigs ds
    e <- ask
    let scope e = foldl (\e' (x, sc) -> extend e' (x, sc)) e sigs

    let (datadecls, functions) = makeFunctions ds
    let datadecls' = map fakeCoerce datadecls -- TODO omg get back to this
    ((++) datadecls' . concat . map unwrapDef) <$> local scope (single functions)

     -- typechecks the functions one by one, extending the environment
     -- with the previously typechecked functions types.
  where single :: [Function ()] -> TC [Function Type]
        single [] = return []
        single (d:ds) = do
            (t, d') <- checkFunction d
            e <- ask
            let scope e = extend e (getName d', generalize e t)
            ds' <- local scope (single ds)
            return $ d' : ds'

        getName (FN n _ _) = n

        unwrapDef :: Function Type -> [Def Type]
        unwrapDef (FN _ Nothing ds)  = ds
        unwrapDef (FN n (Just t) ds) = (DTypeSig n t) : ds

-- groups function clauses together. Assumes definitions are given in the
-- proper order, i.d that they are not defined such as
-- foldl _ b [] = b
-- sum = foldl (+) 0
-- foldl ....
makeFunctions :: [Def ()] -> ([Def ()], [Function ()])
makeFunctions ds =
    let f (DEquation _ n1 _ _) (DEquation _ n2 _ _) = n1 == n2
        f (DTypeSig n1 _) (DEquation _ n2 _ _)      = n1 == n2
        f _ _                                       = False

        groups = groupBy f ds

        isDataDec [(DDataDec _ _ _)] = True
        isDataDec _                  = False

        (datadecs, funs) = partition isDataDec groups
    in (concat datadecs, makeFunctions_ funs)

makeFunctions_ :: [[Def ()]] -> [Function ()]
makeFunctions_ = map makeFun
  where makeFun ((DTypeSig name t):ds)        = FN name (Just t) ds
        makeFun ds@((DEquation _ name _ _):_) = FN name Nothing ds

-- given an annotated definition, return its type
typeOfAnnotatedDef :: Def Type -> TC Type
typeOfAnnotatedDef (DEquation _ _ pats exp) = do
    return $ (function_type (map getPatType pats) (getExpType exp))
-- we only call this after we've already checked for single type definitions
typeOfAnnotatedDef _ = error "shouldn't end up here"

-- is a function recursive? We deduce that it is so if any of the
-- clause bodies uses the identifier bound in the definition.
recursive :: Function a -> Bool
recursive (FN name _ bodies) = any recursiveSingle bodies
  where recursiveSingle (DEquation _ n _ e) = usesVar n e

hasTypeSig :: Function a -> Bool
hasTypeSig (FN _ (Just _) _) = True
hasTypeSig _                 = False

checkFunction :: Function () -> TC (Type, Function Type)
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

checkClause :: Def () -> TC (Def Type)
checkClause (DEquation () name pats exp) = do
    -- get the types and variables bound in the declaration
    patinfo <- mapM (flip checkPattern True) pats
    let pats' = map fst patinfo
    let types = map getPatType pats'
    let vars  = concat $ map snd patinfo
    -- extend the environment with the variables
    exp' <- inEnvMany (map (\(x,t') -> (x, Forall [] t')) vars) (checkExp exp)
    -- get the result type and construct the annotated definition & the complete type
    return $ DEquation (function_type types (getExpType exp')) name pats' exp'

unifyClauses :: Function Type -> TC Type
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
checkPattern :: Pat () -> Bool -> TC (Pat Type, [(Ident, Type)])
checkPattern p allowConstants = case p of
    -- Are constaints allowed? In e.g lambda abstractions we don't
    -- allow constants such as 3
    PConst a c       -> if allowConstants 
                        then return $ (PConst (checkConstType c) c, [])
                        else throwError $ LambdaConstError c

    -- variables are easy! Just generate a fresh type variable for it and return that
    PVar a var       -> fresh >>= \tv -> return (PVar tv var, [(var, tv)])

    -- 0-ary ADT constructor, just look up the type. No local variables are introduced
    PZAdt a con -> do
        t <- lookupCons con
        return $ (PZAdt t con, [])

    -- N-ary ADT constructor
    PNAdt a con pats  -> do
        res <- mapM (flip checkPattern allowConstants) pats

        let pats' = map fst res
        let typs  = map getPatType pats'
        let vars  = concat $ map snd res
        t <- lookupCons con
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
                return $ (PNAdt tv con pats', vars)
            -- otherwise we are not fully applied, and we raise an error.
            False -> throwError $ ConstructorNotFullyApplied con numargs (length pats)

    -- wildcards can have any type they like?
    PWild a          -> fresh >>= \tv -> return (PWild tv, [])

    PNil a           -> return (PNil TNil, [])

    -- The type of a pattern such as (3,5) is (Int, Int). Recursively check what type the
    -- patterns 3 and 5 have, and use those results to build a TTup node.
    PTup a patterns -> do
        res <- mapM (flip checkPattern allowConstants) patterns

        let patterns'  = map fst res
        let pat_types  = map getPatType patterns'
        let vars       = concat $ map snd res
        return (PTup (TTup pat_types) patterns', vars)
    
    PLay a var pat   -> do
        (pat', vars) <- checkPattern pat allowConstants
        let tv = getPatType pat'
        return (PLay tv var pat', (var, tv) : vars)

checkConstType :: Const -> Type
checkConstType const = case const of
    CInt integer  -> int
    CFloat double -> float
    CTrue         -> bool
    CFalse        -> bool
    CNil          -> TNil

checkCases :: Type -> [PatMatch ()] -> TC [PatMatch Type]
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
checkCase :: Type -> PatMatch () -> TC (PatMatch Type)
checkCase t (PM pat e1) = do
    (pat', vars) <- checkPattern pat True
    let t' = getPatType pat'

    uni t t' Nothing
    e1' <- inEnvMany (map (\(x, t'') -> (x, Forall [] t'')) vars) (checkExp e1)
    return $ PM pat' e1'

-- Type check an expression!
checkExp :: Exp () -> TC (Exp Type)
checkExp e = case e of
    ECase _ e1 patterns -> do
        -- check the type of the expression we are casing over
        e1' <- checkExp e1
        let te1 = getExpType e1'

        patterns' <- checkCases te1 patterns
                                                     -- TODO backtrack and see why it has to be reversed here
        let t = getPMType (head (reverse patterns')) -- parser only parses nonempty lists
        return $ ECase t e1' patterns'

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
        return $ ELet te2 p' e1' e2'

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
        return $ ELam (tpat *-> t) pattern' e1'

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
        return $ EIf te2 e1' e2' e3'

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
        return $ EApp tv e1' e2'

    EOr _ e1 e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        -- Are both expressions booleans? Then we are OK!
        uni te1 bool Nothing
        uni te2 bool Nothing
        return $ EOr bool e1' e2'

    EAnd _ e1 e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        uni te1 bool Nothing
        uni te2 bool Nothing
        return $ EAnd bool e1' e2'

    ERel _ e1 op e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        tv <- fresh
        let u1  = (te1 *-> te2 *-> tv)
        
        op' <- relOps op
        let u2 = getRelopVar op'

        uni u1 u2 Nothing
        case op of
            EQC _ -> uniEither [(u1, int *-> int *-> tv),
                                (u1, float *-> float *-> tv),
                                (u1, bool *-> bool *-> tv)]
            otherwise -> uniEither [(u1, int *-> int *-> tv),
                                    (u1, float *-> float *-> tv)]
        return $ ERel tv e1' op' e2'

    EAdd _ e1 op e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        tv <- fresh
        let u1  = (te1 *-> te2 *-> tv)

        op' <- addOps op
        let u2 = getAddopVar op'
 
        uni u1 u2 Nothing
        uniEither [(u1, int *-> int *-> int),
                   (u1, float *-> float *-> float)]
        return $ EAdd tv e1' op' e2'

    EMul _ e1 op e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpType e1'
        let te2 = getExpType e2'
        tv <- fresh
        let u1 = (te1 *-> te2 *-> tv)

        op' <- mulOps op
        let u2 = getMulopVar op'

        uni u1 u2 Nothing
        uniEither [(u1, int *-> int *-> int),
                   (u1, float *-> float *-> float)]
        return $ EMul tv e1' op' e2'

    ENot _ e1 -> do
        e1' <- checkExp e1
        let te1 = getExpType e1'
        -- We can only negate booleans :)
        uni te1 bool Nothing
        return $ ENot bool e1'

    -- Capital letter variables are constructors! Look up the type of it.
    EUVar _ con -> do
        t <- lookupCons con
        return $ EUVar t con

    -- otherwise it is a function or something else, like an x bound by a \x.
    EVar _ var  -> do
        t <- lookupVar var
        return $ EVar t var

    ETup _ exps -> do
        -- check the types of the tuple expressions
        exps' <- mapM checkExp exps
        let texps = map getExpType exps'
        return $ ETup (TTup texps) exps'

    EConst _ const -> case const of
        CInt i   -> return $ EConst int (CInt i)
        CFloat f -> return $ EConst float (CFloat f)
        CTrue    -> return $ EConst bool CTrue
        CFalse   -> return $ EConst bool CFalse
        CNil     -> return $ EConst TNil CNil

addOps :: AddOp () -> TC (AddOp Type)
addOps op = case Map.lookup op addOps_ of
    Just t  -> instantiate t >>= (\t' -> return $ fmap (\_ -> t') op)
    Nothing -> error "see below"

addOps_ :: Map.Map (AddOp ()) Scheme
addOps_ = Map.fromList [
    (Plus   (), let id = Ident "a"
                    a  = TVar id
                in Forall [id] (a *-> a *-> a)),
    (Minus  (), let id = Ident "a"
                    a  = TVar id
                in Forall [id] (a *-> a *-> a))]

mulOps :: MulOp () -> TC (MulOp Type)
mulOps op = case Map.lookup op mulOps_ of
    Just t  -> instantiate t >>= (\t' -> return $ fmap (\_ -> t') op)
    Nothing -> error "see below"    

mulOps_ :: Map.Map (MulOp ()) Scheme
mulOps_ = Map.fromList [
    (Times  (), let id = Ident "a"
                    a = TVar id
                in Forall [id] (a *-> a *-> a)),
    (Div    (), let id = Ident "a"
                    a = TVar id
                in Forall [id] (a *-> a *-> a))]

relOps :: RelOp () -> TC (RelOp Type)
relOps op = case Map.lookup op relOps_ of
    Just t  -> instantiate t >>= (\t' -> return $ fmap (\_ -> t') op)
    Nothing -> error "we should not end up here, how can I enforce this?"

relOps_ :: Map.Map (RelOp ()) Scheme
relOps_ = Map.fromList [
    (LTC  (), let id = Ident "a"
                  a  = TVar id
              in Forall [id] (a *-> a *-> bool)),
    (LEC  (), let id = Ident "a"
                  a  = TVar id
              in Forall [id] (a *-> a *-> bool)),
    (GTC  (), let id = Ident "a"
                  a  = TVar id
              in Forall [id] (a *-> a *-> bool)),
    (GEC  (), let id = Ident "a"
                  a  = TVar id
              in Forall [id] (a *-> a *-> bool)),
    (EQC  (), let id = Ident "a"
                  a  = TVar id
              in Forall [id] (a *-> a *-> bool))]