{-# LANGUAGE FlexibleInstances #-}
module TypecheckTinyCamiot where

import AbsTinyCamiot
import PrintTinyCamiot

import Environment
import Unification
import AstUtils

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

runTC :: TC a -> TEnv -> IO (Either TCError Subst)
runTC tc initEnv = do
    let rd = runReaderT tc initEnv
    let wr = runWriterT rd
    let st = runStateT wr emptyState
    let ex = runExceptT st
    res <- ex
    case res of -- Either TCError (((), [Constraint]), TCState)
        Left err -> return $ Left err
        Right ((_, constraints), _) -> do
            putStrLn $ intercalate "\n" $ map show constraints
            runSolve constraints


{---------------------------------------------------------------------}
{- ******************** -}
-- top level typecheck

typecheck :: [Def ()] -> IO (Either TCError Subst)
typecheck defs = runTC (check defs) emptyEnv

-- collect type sigs
gatherTypeSigs :: [Def ()] -> TC [(Ident, Scheme)]
gatherTypeSigs ds = do
    let typesigs = catMaybes (map go ds)
    let funs     = map fst typesigs
    e           <- ask

    case length funs == length (nub funs) of
        True  -> return $ map (\(n, t) -> (n, (generalize e t))) typesigs
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

check :: [Def ()] -> TC ()
check ds = do
    -- updates state with data types
    gatherDataDecs ds
    -- collect type signatures
    sigs <- gatherTypeSigs ds
    -- typecheck the equations using the extended scope
    let scope e = foldl (\e' (x, sc) -> extend e' (x, sc)) e sigs
    local scope (checkMany ds)

checkMany :: [Def ()] -> TC ()
checkMany [] = return ()
checkMany (d:ds) = do
    mt <- checkSingle d
    case mt of
        Just (x,sc)  -> let scope e = extend e (x, sc)
                        in local scope (checkMany ds)
        nothing      -> checkMany ds

checkSingle :: Def () -> TC (Maybe (Ident, Scheme))
checkSingle d = case d of
    DEquation () name pats exp -> do
        -- get the types and variables bound in the declration
        patinfo <- mapM (flip checkPattern True) pats
        let types = map fst patinfo
        let vars = concat $ map snd patinfo
        -- extend the environment with the variables
        t <- inEnvMany (map (\(x,t') -> (x, Forall [] t')) vars) (checkExp exp)
        -- create the type inferred by the argument types and the result type
        let inferredType = function_type types t
        -- is there a type signature available for this function?
        sig <- lookupTypeSig name
        case sig of
            -- if there is, try to unify the inferred and declared type
            Just assigned -> uni assigned inferredType >> return Nothing
            -- otherwise, just return TODO also add the function and inferred type to the env
            Nothing       -> ask >>= \e -> return $ Just $ (name, generalize e inferredType)
    _ -> return Nothing

-- Input: a pattern
-- output
--   - component 1: Type of the top-level pattern
--   - component 2: List of variables and their type variables created by the pattern
-- e.g pattern Just (a,2) gives us (Maybe (a, Int), [a, Int])
checkPattern :: Pat () -> Bool -> TC (Type (), [(Ident, Type ())])
checkPattern pattern allowConstants = case pattern of
    -- Are constaints allowed? In e.g lambda abstractions we don't
    -- allow constants such as 3
    PConst a c       -> if allowConstants 
                        then return $ (checkConstType c, []) 
                        else throwError $ LambdaConstError c

    -- variables are easy! Just generate a fresh type variable for it and return that
    PVar a var       -> fresh >>= \tv -> return (tv, [(var, tv)])

    -- 0-ary ADT constructor, just look up the type. No local variables are introduced
    PZAdt a con -> do
        t <- lookupCons (Constructor () con)
        return (t, [])

    -- N-ary ADT constructor
    PNAdt a con pats  -> do
        (typs, vars) <- unzip <$> mapM (flip checkPattern allowConstants) (map deAdtPat pats)
        t <- lookupCons (Constructor () con)

        -- What is the creation of the type? E.g Maybe a for Just
        let t'      = unwrap_function t

        -- How many arguments does the constructor expect?
        let numargs = count_arguments t

        -- Is it fully applied? We only pattern match on fully applied constructors.
        case length pats == numargs of
            -- If it is, the type of the pattern is the fully applied type, but with
            -- the type variables in t' exchanged for the inferred types of the
            -- recursive patterns.
            True  -> let (TAdt () con' _) = t' 
                     in return $ (TAdt () con' typs, concat vars)
            -- otherwise we are not fully applied, and we raise an error.
            False -> throwError $ ConstructorNotFullyApplied con numargs (length pats)

    -- wildcards can have any type they like?
    PWild a          -> fresh >>= \tv -> return (tv, [])

    PNil a           -> return (TNil (), [])

    -- The type of a pattern such as (3,5) is (Int, Int). Recursively check what type the
    -- patterns 3 and 5 have, and use those results to build a TTup node.
    PTup a patterns -> do
        res <- mapM (flip checkPattern allowConstants) (map deTupPat patterns)
        let types = map fst res
        let vars  = concat $ map snd res
        return $ (TTup () (map tupType types), vars)
    
    PLay a var pat   -> do
        (tv, tpat) <- checkPattern pat allowConstants
        return $ (tv, (var, tv) : tpat)

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
checkCases :: Type () -> [PatMatch ()] -> TC (Type ())
checkCases t pm = do
    types <- mapM (checkCase t) pm
    uniMany types
    return $ head types

-- Check that a case branch is correctly typed.
-- check the pattern and extend the environment with any
-- variables it might create before we check that the result is well typed.
-- We also verify that the pattern is of the correct type, which is the type
-- given as an argument.
checkCase :: Type () -> PatMatch () -> TC (Type ())
checkCase t (PM () pat e1) = do
    (t', vars) <- checkPattern pat True
    uni t t'
    inEnvMany (map (\(x, t'') -> (x, Forall [] t'')) vars) (checkExp e1) 
    
-- Type check an expression!
checkExp :: Exp () -> TC (Type ())
checkExp e = case e of
    ECase _ e1 patterns -> do
        -- check the type of the expression we are casing over
        te1 <- checkExp e1
        -- check that the branches are of the same result type and that
        -- the pattern type is identical to the expression we cased over.
        checkCases te1 patterns

    ELet _ pattern e1 e2 -> do
        -- Check the type of the pattern
        (tpat, vars) <- checkPattern pattern False
        -- check the type of the expression we are binding to the pattern
        te1 <- checkExp e1
        -- unify them! e.g let (a,b) = Nothing in ... makes no sense
        uni tpat te1
        -- extend the environment with the new variable(s) and check e2
        inEnvMany (map (\(x,t') -> (x, Forall [] t')) vars) (checkExp e2)

    -- TODO
    ELetR _ pattern e1 e2 -> undefined

    ELam _ pattern e1 -> do
        -- check the pattern! We do not allow constants here.
        (tpat, vars) <- checkPattern pattern False
        -- extend the environment with the variables bound by the lambda and
        -- check the function body.
        t <- inEnvMany (map (\(x,t') -> (x, Forall [] t')) vars) (checkExp e1)
        -- The whole expressions is of the type
        -- (type of pattern) -> (type of expression)
        return $ tpat *-> t

    EIf _ e1 e2 e3 -> do
        -- check all three expression
        te1 <- checkExp e1
        te2 <- checkExp e2
        te3 <- checkExp e3
        -- unify the first one with bool
        uni te1 bool
        -- unify the other two, both branches must have the same type
        uni te2 te3
        -- return te2 (or te3, doesn't matter)
        return te2

    EApp a e1 e2 -> do
        -- check the type of the (hopefully) function we are applying
        te1 <- checkExp e1
        -- check the type of the argument to the function
        te2 <- checkExp e2
        -- generate a fresh type variable for the 'result'
        tv <- fresh
        -- see if it is possible to unify the types. Essentially
        -- checking if the first argument of te1 is te2.
        uni te1 (te2 *-> tv)
        return tv

    EOr _ e1 e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        -- Are both expressions booleans? Then we are OK!
        uni te1 bool
        uni te2 bool
        return bool

    EAnd _ e1 e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        uni te1 bool
        uni te2 bool
        return bool

    ERel _ e1 op e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        tv <- fresh
        -- u2 will become e.g int -> int -> int for (+)
        -- is it possible to unify te1 -> te2 -> tv and
        --                         int -> int -> int?
        let u1 = (te1 *-> te2 *-> tv)
            u2 = relOps Map.! op
        uni u1 u2
        return tv

    EAdd _ e1 op e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        tv <- fresh
        let u1 = (te1 *-> te2 *-> tv)
            u2 = addOps Map.! op
        uni u1 u2
        return tv

    EMul _ e1 op e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        tv <- fresh
        let u1 = (te1 *-> te2 *-> tv)
            u2 = mulOps Map.! op
        uni u1 u2
        return tv

    ENot _ e1 -> do
        te1 <- checkExp e1
        -- We can only negate booleans :)
        uni te1 bool
        return bool

    -- Capital letter variables are constructors! Look up the type of it.
    EUVar _ con -> lookupCons (Constructor () con)
    -- otherwise it is a function or something else, like an x bound by a \x.
    EVar _ var  -> lookupVar var

    ETup _ tupExps -> do
        let exps = map deTupExp tupExps
        -- check the types of the tuple expressions
        texps <- mapM checkExp exps
        -- simply build a tuple type of the inferred types. Nothing to unify, tuples
        -- are polymorphic in their contents.
        return $ TTup () (map tupType texps)

    EConst _ const -> case const of
        CInt a i   -> return int
        CFloat a f -> return float
        CTrue a    -> return bool
        CFalse a   -> return bool
        CNil a     -> return $ TNil ()

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
