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
                      True  -> return $ (con, t)
                      False -> throwError $ TypeArityError con' (map (TVar ()) tvars) vars
                  False -> throwError $ WrongConstructorGoal con goal (TAdt () typ (map (TVar ()) tvars))
              _ -> throwError $ WrongConstructorGoal con goal (TAdt () typ (map (TVar ()) tvars))
      
      getGoal (TLam _ _ r) = getGoal r
      getGoal t            = t

{- ******************** -}
-- typecheck

check :: [Def ()] -> TC ()
check ds = do
    gatherDataDecs ds
    sigs <- gatherTypeSigs ds
    let scope e = foldl (\e' (x, sc) -> extend e' (x, sc)) e sigs
    --let m = foldl (\ma d -> checkSingle d ma) (return ()) ds
    local scope (checkMany ds)

checkMany :: [Def ()] -> TC ()
checkMany [] = return ()
checkMany (d:ds) = do
    checkSingle d
    checkMany ds

checkSingle :: Def () -> TC ()
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
            Just assigned -> uni assigned inferredType
            -- otherwise, just return TODO also add the function and inferred type to the env
            Nothing       -> return ()
                       --let scope e = let e' = restrict e name 
                       --              in extend e' (name, generalize e' inferredType)
                       --in local scope m
    _ -> return ()

-- Input: a pattern
-- output
--   - component 1: Type of the top-level pattern
--   - component 2: List of variables and their type variables created by the pattern
-- TODO Maybe the second component should be name and type scheme?
-- TODO rewrite this behemoth
checkPattern :: Pat () -> Bool -> TC (Type (), [(Ident, Type ())])
checkPattern pattern allowConstants = case pattern of
    PConst a c       -> if allowConstants 
                        then return $ (checkConstType c, []) 
                        else throwError $ LambdaConstError c

    PVar a var       -> fresh >>= \tv -> return (tv, [(var, tv)])

    PZAdt a con -> do
        t <- lookupCons (Constructor () con)
        return (t, [])

    PNAdt a con pats  -> do
        (typs, vars) <- unzip <$> mapM (flip checkPattern allowConstants) (map deAdtPat pats)
        t <- lookupCons (Constructor () con)
        let t'      = unwrap_function t
        let numargs = count_arguments t

        -- is the constructor correctly applied?
        case length pats == numargs of
            True  -> let (TAdt () con' _) = t' 
                     in return $ (TAdt () con' typs, concat vars)
            False -> throwError $ ConstructorNotFullyApplied con numargs (length pats)

    PWild a          -> fresh >>= \tv -> return (tv, [])

    PNil a           -> return (TNil (), [])

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

checkCases :: Type () -> [PatMatch ()] -> TC (Type ())
checkCases t pm = do
    types <- mapM (checkCase t) pm
    uniMany types
    return $ head types

checkCase :: Type () -> PatMatch () -> TC (Type ())
checkCase t (PM () pat e1) = do
    (t', vars) <- checkPattern pat True
    uni t t'
    inEnvMany (map (\(x, t'') -> (x, Forall [] t'')) vars) (checkExp e1) 
    

checkExp :: Exp () -> TC (Type ())
checkExp e = case e of
    ECase _ e1 patterns -> do
        te1 <- checkExp e1
        checkCases te1 patterns

    ELet _ pattern e1 e2 -> do
        (tpat, vars) <- checkPattern pattern False
        te1 <- checkExp e1
        uni tpat te1
        inEnvMany (map (\(x,t') -> (x, Forall [] t')) vars) (checkExp e2)

    -- TODO
    ELetR _ pattern e1 e2 -> undefined

    ELam _ pattern e1 -> do
        -- TODO Maybe we want checkLambdaPattern to
        -- return Schemes with the proper type variables
        (tpat, vars) <- checkPattern pattern False
        t <- inEnvMany (map (\(x,t') -> (x, Forall [] t')) vars) (checkExp e1)
        return $ TLam () tpat t

    EIf _ e1 e2 e3 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        te3 <- checkExp e3
        uni te1 bool
        uni te2 te3
        return te2

    EApp a e1 e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        tv <- fresh
        uni te1 (TLam () te2 tv)
        return tv

    EOr _ e1 e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
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
        let u1 = TLam () te1 (TLam () te2 tv)
            u2 = relOps Map.! op
        uni u1 u2
        return tv

    EAdd _ e1 op e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        tv <- fresh
        let u1 = TLam () te1 (TLam () te2 tv)
            u2 = addOps Map.! op
        uni u1 u2
        return tv

    EMul _ e1 op e2 -> do
        te1 <- checkExp e1
        te2 <- checkExp e2
        tv <- fresh
        let u1 = TLam () te1 (TLam () te2 tv)
            u2 = mulOps Map.! op
        uni u1 u2
        return tv

    ENot _ e1 -> do
        te1 <- checkExp e1
        uni te1 bool
        return bool

    EUVar _ con -> lookupCons (Constructor () con)
    EVar _ var  -> lookupVar var

    ETup _ tupExps -> do
        let exps = map deTupExp tupExps
        texps <- mapM checkExp exps
        return $ TTup () (map tupType texps)

    EConst _ const -> case const of
        CInt a i   -> return int
        CFloat a f -> return float
        CTrue a    -> return bool
        CFalse a   -> return bool
        CNil a     -> undefined

-- TODO we can remove the 'duplice' operators and try to overload them
-- when we know how to attempt the unification when the types it can be
-- unified with are not arbitrary, but actually a subset of all types.

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