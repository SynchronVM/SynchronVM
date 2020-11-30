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
import Typechecker.Substitution hiding (Subst)
import Typechecker.Environment
import Typechecker.AstUtils
import Typechecker.TCUtils

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.Maybe
import Data.List
import Data.Foldable ()
import qualified Data.Map as Map
import qualified Data.Set as Set

import HindleyMilner.TypeInference
import HindleyMilner.HM

typecheck :: [Def ()] -> IO (Either TCError ([Def Type], Subst))
typecheck tc = do
    res <- runTC (checkProgram tc) emptyEnv
    case res of
        Left err -> return $ Left err
        Right (tc', constraints) -> do
                    let esubst = runUnify (convCons constraints)
                    case esubst of
                        Just subst -> return (Right (tc',subst))
                        Nothing    -> undefined 

convCons :: [Constraint] -> [[(Type, Type)]]
convCons []                   = []
convCons (C (t1, t2, _) : cs) = [(t1,t2)] : convCons cs
convCons (C2 c : cs)          = map (\(C (t1,t2,_)) -> (t1,t2) ) c : convCons cs

{---------------------------------------------------------------------}
{- ******************** -}
-- top level typecheck

-- | Traverses the program and returns a list of type signatures and their variables.
gatherTypeSigs :: [Def ()] -> TC [(Ident, Scheme)]
gatherTypeSigs ds = do
    let typesigs = catMaybes (map go ds)
    let funs     = map fst typesigs
    e           <- ask

    case length funs == length (nub funs) of
        True  -> return $ map (\(n, t) -> (n, (generalize e t))) typesigs
        False -> throwError $ DuplicateTypeSig $ head (intersect funs (nub funs))

  where go (DTypeSig fun t) = Just (fun, t)
        go _                   = Nothing

-- | Traverses a program and puts the data declarations in the type checking environment
gatherDataDecs :: [Def ()] -> TC ()
gatherDataDecs [] = return ()
gatherDataDecs (d:ds) = case d of
    DDataDec typ tvars cons -> do
        constructors <- mapM (consDecToType typ tvars) cons
        tryInsert constructors
        gatherDataDecs ds
    _ -> gatherDataDecs ds
  where
      -- | Tries to insert a constructor in the environment. Checks for name collisions.
      tryInsert :: [(UIdent, Type)] -> TC ()
      tryInsert []         = return ()
      tryInsert ((c,t):xs) = do
          env <- get
          e   <- ask
          let cons = constructors env
          case Map.lookup c cons of
              Just t' -> throwError $ DuplicateConstructor c t
              Nothing -> put (env { constructors = Map.insert c (generalize e t) cons}) >> tryInsert xs

      {-- | Given a type constructor, a list of type variables and a constructor
      declaration, this function will check that the type of the constructor is
      well formed.
      -}
      consDecToType 
          :: UIdent            -- ^ Type constructor, e.g Maybe
          -> [Ident]           -- ^ Type variables, e.g b
          -> ConstructorDec    -- ^ Constructor declaration, e.g Just : a -> Maybe a
          -> TC (UIdent, Type)
      consDecToType typ tvars (ConstDec con t) =
          let goal = getGoal t
          in case goal of
              (TAdt con' vars) -> case typ == con' of
                  True  -> case length tvars == length vars of
                      True  -> let isOk = Set.isSubsetOf (ftv t) (Set.fromList tvars)
                                   diff = Set.difference (ftv t) (Set.fromList tvars)
                               in case isOk of
                                    True  -> return $ (con, t)
                                    False -> throwError $ UnboundTypeVariable tvars (Set.toList diff)
                      False -> throwError $ TypeArityError con' (map TVar tvars) vars
                  False -> throwError $ WrongConstructorGoal con goal (TAdt typ (map TVar tvars))
              _ -> throwError $ WrongConstructorGoal con goal (TAdt typ (map TVar tvars))
      
      {-- | If applied to a function type, this function will remove the arguments and
      return the result type.
      -}
      getGoal :: Type -> Type
      getGoal (TLam  _ r) = getGoal r
      getGoal t            = t

{- ******************** -}
-- typecheck

data Function a = FN Ident (Maybe Type) [Def a]

-- | Given a list of definitions, return a list of typechecked and annotated definitions.
checkProgram :: [Def ()] -> TC [Def Type]
checkProgram ds = do
    gatherDataDecs ds
    sigs <- gatherTypeSigs ds
    e <- ask
    let scope e = foldl (\e' (x, sc) -> extend e' (x, sc)) e sigs

    let (datadecls, functions) = makeFunctions ds
    let datadecls' = map fakeCoerce datadecls -- TODO omg get back to this
    ((++) datadecls' . concat . map unwrapDef) <$> local scope (single functions)

  where
        {-- | This is a recursive function that will typecheck one function in the
        list, extend the local environment with the functions name and the inferred
        type, and then recursively tries to typecheck the rest of the functions.
        -}
        single :: [Function ()] -> TC [Function Type]
        single [] = return []
        single (d:ds) = do
            (t, d') <- checkFunction d
            e <- ask
            let scope e = extend e (getName d', generalize e t)
            ds' <- local scope (single ds)
            return $ d' : ds'

        -- | Given a `Function a`, returns the name of the function.
        getName :: Function a -> Ident
        getName (FN n _ _) = n

        {-- | Given a value of type Function Type, constructs a list of definitions
        that represent the same information.
        -}
        unwrapDef :: Function Type -> [Def Type]
        unwrapDef (FN _ Nothing ds)  = ds
        unwrapDef (FN n (Just t) ds) = (DTypeSig n t) : ds

        {-- | A bad way of changing the 'phantom' type in `Def`. Meant to be called only
        on data type declarations and type signature declarations, who carry no value of
        type a.
        -}
        fakeCoerce :: Def a -> Def Type
        fakeCoerce (DDataDec tyvar vars cons) = DDataDec tyvar vars cons
        fakeCoerce (DTypeSig name sig) = DTypeSig name sig
        fakeCoerce otherwise = error "should not be invoked"

{-- | Given a program as a list of definitions, return a pair where the first component
is the data type declarations and the second component is a list of all the functions,
assembled as objects of type `Function ()`.
-}
makeFunctions
    {-- | Input program. Layout is assumed to be of a good nature. By this it is meant
    that function clauses follow each other and a function definition follows immediately
    after its type signature (if there is any).
    -}
    :: [Def ()]
    -> ([Def ()], [Function ()])
makeFunctions ds =
    let f (DEquation _ n1 _ _) (DEquation _ n2 _ _) = n1 == n2
        f (DTypeSig n1 _) (DEquation _ n2 _ _)      = n1 == n2
        f _ _                                       = False

        groups = groupBy f ds

        isDataDec [(DDataDec _ _ _)] = True
        isDataDec _                  = False

        (datadecs, funs) = partition isDataDec groups
    in (concat datadecs, (map makeFun) funs)
  where
      {-- | Wraps a list of definitions up as a Function type. The idea is that it is
      more convenient to pass objects of this type around than to keep operating
      on lists.
      -}
      makeFun :: [Def ()] -> Function ()
      makeFun ((DTypeSig name t):ds)        = FN name (Just t) ds
      makeFun ds@((DEquation _ name _ _):_) = FN name Nothing ds

{-- | This function returns true if the given Function is recursive. It performs the
recursive check by seeing if the functions name appears in its body. Might be inefficient
if function bodies are very big.
-}
recursive :: Function a -> Bool
recursive (FN name _ bodies) = any recursiveSingle bodies
  where
      {-- | Checks a single function definition to see if it is recursive. A function
      may have several function clauses. E.g in a base case we would not see the name.
      -}
      recursiveSingle :: Def a -> Bool
      recursiveSingle (DEquation _ n _ e) = usesVar n e

{-- | Given a function that has not been type checked, return a pair where the first
element is the type of the function, and the second element is the type-annotated
function. If the functions is recursive but lacks a type signature, we raise an error.
-}
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
  where
      -- | Returns true if the function has a type signature.
      hasTypeSig :: Function a -> Bool
      hasTypeSig (FN _ (Just _) _) = True
      hasTypeSig _                 = False

-- | This function typechecks a single definition.
checkClause :: Def () -> TC (Def Type)
checkClause (DEquation () name pats exp) = do
    -- get the types and variables bound in the declaration
    patinfo <- mapM (flip checkPattern True) pats
    let pats' = map fst patinfo
    let types = map getPatVar pats'
    let vars  = concat $ map snd patinfo
    -- extend the environment with the variables
    exp' <- inEnvMany (map (\(x,t') -> (x, Forall [] t')) vars) (checkExp exp)
    -- get the result type and construct the annotated definition & the complete type
    return $ DEquation (functionType types (getExpVar exp')) name pats' exp'

{-- | This function will receive a function as an argument, and will try to unify all
the functions clauses. It will then return the unified type of the function clauses.
The purpose is to make sure that all definitions of a function is of the same type.
-}
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
                t' <- typeOfDef d
                uni instantiated t' (Just (okSigTest instantiated))) ds
            return typ
        Nothing  -> do
            types <- mapM typeOfDef ds
            uniMany types (Just noSigTest)
            return (last types)
  where
    -- | If a definition has been type checked, return the type of it.
    typeOfDef :: Def Type -> TC Type
    typeOfDef (DEquation _ _ pats exp) = return $
                                  (functionType (map getPatVar pats) (getExpVar exp))
    typeOfDef _ = error "" -- should not end up here

{-- | Type check a pattern. The result is a pair where the first element is the type
annotated pattern, and the second component is a list of pairs of variables and their
types. E.g in the pattern (x,y) we don't just encounter the pattern (x,y) of type (a,b),
but we also encounter the variable x of type a, and the variable y of type b. These must
be inserted into the environment.
-}
checkPattern
    :: Pat ()  -- ^ Input pattern
    {-- | Are constants allowed in the pattern? They are when e.g pattern matching in
    a function definition, but not when we create a lambda abstraction.
    -}
    -> Bool
    -> TC (Pat Type, [(Ident, Type)])
checkPattern p allowConstants = case p of
    PConst a c -> if allowConstants 
                    then return     $ (PConst (checkConstType c) c, [])
                    else throwError $ LambdaConstError c

    PVar a var       -> fresh >>= \tv -> return (PVar tv var, [(var, tv)])

    PZAdt a con -> do
        t <- lookupCons con
        return $ (PZAdt t con, [])

    PNAdt a con pats  -> do
        res <- mapM (flip checkPattern allowConstants) pats

        let pats' = map fst res
        let typs  = map getPatVar pats'
        let vars  = concat $ map snd res
        t <- lookupCons con
        let numargs = countArguments t

        let test t1 t2 = if t1 /= t2
                         then Just $ PatternTypeError p t1 t2
                         else Nothing

        -- Is it fully applied? We only pattern match on fully applied constructors.
        case length pats == numargs of
            True  -> do
                tv <- fresh
                uni t (functionType typs tv) (Just test)
                return $ (PNAdt tv con pats', vars)
            False -> throwError $ ConstructorNotFullyApplied con numargs (length pats)

    -- wildcards can have any type they like?
    PWild a          -> fresh >>= \tv -> return (PWild tv, [])

    PNil a           -> return (PNil TNil, [])

    PTup a patterns -> do
        res <- mapM (flip checkPattern allowConstants) patterns

        let patterns'  = map fst res
        let pat_types  = map getPatVar patterns'
        let vars       = concat $ map snd res
        return (PTup (TTup pat_types) patterns', vars)
    
    PLay a var pat   -> do
        (pat', vars) <- checkPattern pat allowConstants
        let tv = getPatVar pat'
        return (PLay tv var pat', (var, tv) : vars)

-- | This functions returns the type of the argument constant.
checkConstType :: Const -> Type
checkConstType const = case const of
    CInt integer  -> int
    CFloat double -> float
    CTrue         -> bool
    CFalse        -> bool
    CNil          -> TNil

-- | This function will typecheck the branches of a case expression..
checkCases :: Type           -- ^ Type of the cased expression 
           -> [PatMatch ()]  -- ^ Case branches of the form Pat -> Exp
           -> TC [PatMatch Type]
checkCases t pm = do
    pm' <- mapM (checkCase t) pm
    let types = map getPMType pm'
    uniMany types Nothing
    return $ pm'
  where
     {-- | Check that a case branch is correctly typed. This function will type check the
     pattern and extend the environment with the variables bound by the pattern before
     the expression is typechecked.
     -}
     checkCase :: Type         -- ^ The pattern must be of this type 
               -> PatMatch ()  -- ^ The case branch to type check
               -> TC (PatMatch Type)
     checkCase t (PM pat e1) = do
         (pat', vars) <- checkPattern pat True
         let t' = getPatVar pat'

         uni t t' Nothing
         e1' <- inEnvMany (map (\(x, t'') -> (x, Forall [] t'')) vars) (checkExp e1)
         return $ PM pat' e1'

-- | This function will check the type of an expression.
checkExp :: Exp () -> TC (Exp Type)
checkExp e = case e of
    ECase _ e1 patterns -> do
        -- check the type of the expression we are casing over
        e1' <- checkExp e1
        let te1 = getExpVar e1'

        patterns' <- checkCases te1 patterns
                                                     -- TODO backtrack and see why it has to be reversed here
        let t = getPMType (head (reverse patterns')) -- parser only parses nonempty lists
        return $ ECase t e1' patterns'

    ELet _ p e1 e2 -> do
        -- Check the type of the pattern
        (p', vars) <- checkPattern p False
        let tpat = getPatVar p'
        -- check the type of the expression we are binding to the pattern
        e1' <- checkExp e1
        let te1 = getExpVar e1'
        -- unify them! e.g let (a,b) = Nothing in ... makes no sense
        uni tpat te1 Nothing
        -- extend the environment with the new variable(s) and check e2
        e2' <- inEnvMany (map (\(x,t') -> (x, Forall [] t')) vars) (checkExp e2)
        let te2 = getExpVar e2'
        return $ ELet te2 p' e1' e2'

    -- TODO
    ELetR _ pattern e1 e2 -> undefined

    ELam _ pattern e1 -> do
        -- check the pattern! We do not allow constants here.
        (pattern', vars) <- checkPattern pattern False
        let tpat = getPatVar pattern'
        -- extend the environment with the variables bound by the lambda and
        -- check the function body.
        e1' <- inEnvMany (map (\(x,t') -> (x, Forall [] t')) vars) (checkExp e1)
        let t = getExpVar e1'
        -- The whole expressions is of the type
        -- (type of pattern) -> (type of expression)
        return $ ELam (tpat *-> t) pattern' e1'

    EIf _ e1 e2 e3 -> do
        -- check all three expression
        e1' <- checkExp e1
        e2' <- checkExp e2
        e3' <- checkExp e3
        let te1 = getExpVar e1'
        let te2 = getExpVar e2'
        let te3 = getExpVar e3'
        -- unify the first one with bool
        uni te1 bool Nothing
        -- unify the other two, both branches must have the same type
        uni te2 te3 Nothing
        -- return te2 (or te3, doesn't matter)
        return $ EIf te2 e1' e2' e3'

    EApp a e1 e2 -> do
        -- check the type of the (hopefully) function we are applying
        e1' <- checkExp e1
        let te1 = getExpVar e1'
        -- check the type of the argument to the function
        e2' <- checkExp e2
        let te2 = getExpVar e2'
        -- generate a fresh type variable for the 'result'
        tv <- fresh
        -- see if it is possible to unify the types. Essentially
        -- checking if the first argument of te1 is te2.
        uni te1 (te2 *-> tv) Nothing
        return $ EApp tv e1' e2'

    EOr _ e1 e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpVar e1'
        let te2 = getExpVar e2'
        -- Are both expressions booleans? Then we are OK!
        uni te1 bool Nothing
        uni te2 bool Nothing
        return $ EOr bool e1' e2'

    EAnd _ e1 e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpVar e1'
        let te2 = getExpVar e2'
        uni te1 bool Nothing
        uni te2 bool Nothing
        return $ EAnd bool e1' e2'

    ERel _ e1 op e2 -> do
        e1' <- checkExp e1
        e2' <- checkExp e2
        let te1 = getExpVar e1'
        let te2 = getExpVar e2'
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
        let te1 = getExpVar e1'
        let te2 = getExpVar e2'
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
        let te1 = getExpVar e1'
        let te2 = getExpVar e2'
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
        let te1 = getExpVar e1'
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
        let texps = map getExpVar exps'
        return $ ETup (TTup texps) exps'

    EConst _ const -> case const of
        CInt i   -> return $ EConst int (CInt i)
        CFloat f -> return $ EConst float (CFloat f)
        CTrue    -> return $ EConst bool CTrue
        CFalse   -> return $ EConst bool CFalse
        CNil     -> return $ EConst TNil CNil

-- | This function will typecheck an addition operator.
addOps :: AddOp () -> TC (AddOp Type)
addOps op = case Map.lookup op addOps_ of
    Just t  -> instantiate t >>= (\t' -> return $ fmap (\_ -> t') op)
    Nothing -> error "see below"
  where
      -- | This is a constant map that maps untyped operator to type schemes.
      addOps_ :: Map.Map (AddOp ()) Scheme
      addOps_ = Map.fromList [
        (Plus   (), let id = Ident "a"
                        a  = TVar id
                    in Forall [id] (a *-> a *-> a)),
        (Minus  (), let id = Ident "a"
                        a  = TVar id
                    in Forall [id] (a *-> a *-> a))]

-- | This function will typecheck a multiplication operator.
mulOps :: MulOp () -> TC (MulOp Type)
mulOps op = case Map.lookup op mulOps_ of
    Just t  -> instantiate t >>= (\t' -> return $ fmap (\_ -> t') op)
    Nothing -> error "see below"    
  where
      -- | This is a constant map that maps untyped operators to type schemes.
      mulOps_ :: Map.Map (MulOp ()) Scheme
      mulOps_ = Map.fromList [
          (Times  (), let id = Ident "a"
                          a = TVar id
                      in Forall [id] (a *-> a *-> a)),
          (Div    (), let id = Ident "a"
                          a = TVar id
                      in Forall [id] (a *-> a *-> a))]

-- | This function will typecheck a relation operator.
relOps :: RelOp () -> TC (RelOp Type)
relOps op = case Map.lookup op relOps_ of
    Just t  -> instantiate t >>= (\t' -> return $ fmap (\_ -> t') op)
    Nothing -> error "we should not end up here, how can I enforce this?"
  where
      -- | This is a constant map that maps untyped operators to type schemes.
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