module Desugaring.Desugar where

import Typechecker.AstUtils
import qualified Parser.AbsTinyCamiot as AST
import Control.Monad.State
import Data.Foldable
import Data.Maybe
import Desugaring.AST
import Data.List

import qualified Data.Map as Map

import System.IO.Unsafe

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

{- ********** Actual desugaring ********** -}

data DSState = DSState {
               counter :: Int
             , constructorfuncs :: Map.Map AST.UIdent AST.Ident
             }
type DS a = State DSState a

desugar :: Int -> [AST.Def AST.Type] -> SExp SType
desugar state defs = runDS state $ do
    if length datadecs > 0
        then do datadecs <- desugarADTs datadecs
                program  <- desugarFunctions funs' (fromJust main)
                return $ datadecs program
        else case main of
               Nothing -> error "No main function present!\n\n"
               Just main' -> desugarFunctions funs' (main')
  where
      funs  = groupAsFunctions defs
      funs' = filter (\(d:_) -> case d of
                    AST.DMutRec _          -> True
                    AST.DDataDec _ _ _     -> False
                    AST.DTypeSig id _      -> id /= AST.Ident "main"
                    AST.DEquation _ id _ _ -> id /= AST.Ident "main") funs
      main  = find (\(d:_) -> case d of
                    AST.DMutRec _          -> False
                    AST.DDataDec _ _ _     -> False
                    AST.DTypeSig id _      -> id == AST.Ident "main"
                    AST.DEquation _ id _ _ -> id == AST.Ident "main")
                  funs
      datadecs = filter (\d -> case d of
          AST.DDataDec _ _ _ -> True
          _                  -> False) defs

runDS :: Int -> DS a -> a
runDS state ds = evalState ds $ DSState state Map.empty

fresh :: DS AST.Ident
fresh = do
    st <- get
    put $ st { counter = counter st + 1}
    return $ AST.Ident $ "v" ++ show (counter st)

-- Expects input to be a non-empty list
desugarADTs :: [AST.Def AST.Type] -> DS (SExp SType -> SExp SType)
desugarADTs decs = do
    decs' <- reverse <$> mapM desugarADT decs
    foldlM (\f g -> return (g . f)) (head decs') (tail decs')
  where
      desugarADT :: AST.Def AST.Type -> DS (SExp SType -> SExp SType)
      desugarADT (AST.DDataDec uid _ cons) = do
          cons' <- reverse <$> mapM desugarConstructor cons
          foldlM (\f g -> return (g . f)) (head cons') (tail cons')

      desugarConstructor :: AST.ConstructorDec -> DS (SExp SType -> SExp SType)
      desugarConstructor (AST.ConstDec uid t) = do
          -- get the types of the arguments to the constructor
          let typeargs = typearguments t
          -- pair them up with freshly generated names
          varpairs <- mapM (\t -> fresh >>= \id -> return (id, desugarType t)) typeargs
          -- turn them into SPat's
          let patvars = map (\(id,t) -> SPVar t id) varpairs
          -- turn them into SExp's
          let expvars = map (\(id,t) -> SEVar t id) varpairs
          -- turn them into a SExp SType
          let adttype = desugarType $ unwrapFunction t
          let adtexp  = SETag adttype uid $ case expvars of
                   []  -> Nothing
                   [x] -> Just x
                   _   -> Just $ foldl (\a e -> let t1 = getSExpVar e
                                                    t2 = getSExpVar a
                                                in SETup (STTup t1 t2) e a)
                                      (last expvars)
                                      (tail (reverse expvars))
          -- bind the variables in adtexp in lambdas and return the resulting expression
          let res     = foldl (\e p -> let t1 = getSExpVar e
                                           (SPVar t2 _) = p
                                       in SELam (STLam t1 t2) p e) adtexp (reverse patvars)
          -- new name for the function we just created which we will then
          -- insert in the environent as a mapping from the constructor to the new name.
          id <- fresh
          modify $ (\(DSState c m) -> DSState c (Map.insert uid id m))
          let fun = SPVar (getSExpVar res) id
          return $ \e -> SELet (getSExpVar e) fun res e

desugarMutRecs :: (AST.Def AST.Type, [AST.Def AST.Type])
               -> DS (SPat SType, SExp SType)
desugarMutRecs (AST.DTypeSig ident t, defs) = do
  dsExp <- desugarFunction' defs
  return (SPVar (desugarType t) ident, dsExp)

desugarFunctions :: [[AST.Def AST.Type]] -> [AST.Def AST.Type] -> DS (SExp SType)
desugarFunctions functions main = do
    m <- gets constructorfuncs
    case functions of
        [] -> return $ desugarMain m
        ([AST.DMutRec tydefs]:funs) -> do
          tydefs' <- mapM desugarMutRecs tydefs
          rest <- desugarFunctions funs main
          return $ SEMutR (typeof rest) tydefs' rest
        (fun:funs) -> case funs of
                [] -> do fun' <- desugarFunction fun
                         return $ fun' (desugarMain m)
                _  -> do fun' <- desugarFunction fun
                         rest <- desugarFunctions funs main
                         return $ fun' rest
  where
    desugarMain :: Map.Map AST.UIdent AST.Ident -> SExp SType
    desugarMain m = desugarExp m (findMainBody main)

    findMainBody [] = error $ "main body not found"
    findMainBody ((AST.DEquation _ id _ body):xs)
      | id == (AST.Ident "main") = body
      | otherwise = findMainBody xs
    findMainBody (_:xs) = findMainBody xs

    typeof = getSExpVar

-- | Desugar a single function, represented as a list of definitions.
desugarFunction :: [AST.Def AST.Type] -> DS (SExp SType -> SExp SType)
desugarFunction defs = do
    m <- gets constructorfuncs
    if numargs defs > 0
        then do
            -- new arguments for the function
            args' <- replicateM (numargs defs) fresh

            -- fetch the types the arguments should have
            let argtyps   = let (AST.DEquation _ _ args _) = head defs' in map getPatVar args
            -- create typed variables out of the generated variable names and the fetched types
            let typedargs = zipWith (\id typ -> AST.EVar typ id) args' argtyps
            -- create a tuple out of the new typed variables and simplify it
            let argtup    = desugarExp m (AST.ETup (AST.TTup argtyps) typedargs)

            -- case branches representing the different equations
            let casebranches = map (uncurry SPM) (branches m defs)
            -- result type of case expression
            let casetype     = getSExpVar (snd (head (branches m defs)))
            -- the actual case expression
            let caseExp      = SECase casetype argtup casebranches
            -- turn all the variables into patterns and bind them in lambdas
            let stypedargs   = map (\(AST.EVar t v) -> SPVar (desugarType t) v) typedargs
            let resultExp    = foldl (\body var@(SPVar a _) -> 
                                        SELam (STLam a (getSExpVar body)) var body)
                                    caseExp
                                    (reverse stypedargs)

            -- type and name of function to simplify
            let (typ, name) = typeAndName
            -- return either the recursive-tagged one or a normal let binding
            return $ if recursive defs
                then SELetR typ (SPVar typ name) resultExp
                else SELet  typ (SPVar typ name) resultExp
        else do let (typ, name)  = typeAndName
                let casebranches = branches m defs
                let let_ = case recursive defs of
                             True -> SELetR
                             False -> SELet
                case casebranches of
                    [(_,x)] -> return $ let_ typ (SPVar typ name) x
                    _   -> error "we should not end up here"
  where
      -- We don't care about the type signature any more, so we drop it if there is one.
      defs' :: [AST.Def AST.Type]
      defs' = case head defs of
          AST.DEquation _ _ _ _ -> defs
          AST.DTypeSig _ _      -> tail defs
    
      -- Name of the function
      typeAndName :: (SType, AST.Ident)
      typeAndName = case head defs of
          AST.DTypeSig id t      -> (desugarType t, id)
          AST.DEquation t id _ _ -> (desugarType t, id)

      -- A list of pairs of (patterns, body) representing an equation such as f patterns = body
      branches :: Map.Map AST.UIdent AST.Ident -> [AST.Def AST.Type] -> [(SPat SType, SExp SType)]
      branches m []                                = []
      branches m (AST.DTypeSig _ _: ds)            = branches m ds
      branches m (AST.DEquation t id args body:ds) =
          {- We are going to case match on the arguments, so we turn them into
          a long tuple instead (if there are more than one). -}
          let argtup = case length args of
                         1 -> head args
                         _ -> AST.PTup (AST.TTup (map getPatVar args)) args
              -- turn the tuple into a simplified tuple
              args'  = desugarPat argtup
              -- simplify the body
              body'  = desugarExp m body
          in (args', body') : branches m ds

      numargs :: [AST.Def AST.Type] -> Int
      numargs []                           = error "no definitions - should not end up here"
      numargs (AST.DTypeSig _ _:ds)        = numargs ds
      numargs (AST.DEquation _ _ args _:_) = length args

desugarExp :: Map.Map AST.UIdent AST.Ident -> AST.Exp AST.Type -> SExp SType
desugarExp m e = case e of
    AST.ECase a e' pms  -> let a'   = desugarType a
                               e''  = desugarExp m e'
                               pms' = map (desugarPM m) pms
                           in SECase a' e'' pms'
    AST.ELet a p e1 e2  -> let a'  = desugarType a
                               p'  = desugarPat p
                               e1' = desugarExp m e1
                               e2' = desugarExp m e2
                           in SELet a' p' e1' e2'
    AST.ELetR a p e1 e2 -> let a'  = desugarType a
                               p'  = desugarPat p
                               e1' = desugarExp m e1
                               e2' = desugarExp m e2
                           in SELetR a' p' e1' e2'
    AST.ELam a p e'     -> let a'  = desugarType a
                               p'  = desugarPat p
                               e'' = desugarExp m e'
                           in SELam a' p' e''
    -- If becomes a case expression
    AST.EIf a e1 e2 e3  -> let a'  = desugarType a
                               e1' = desugarExp m e1
                               e2' = desugarExp m e2
                               e3' = desugarExp m e3
                           in SEIf a' e1' e2' e3'
    AST.EApp a e1 e2    -> let a'  = desugarType a
                               e1' = desugarExp m e1
                               e2' = desugarExp m e2
                           in SEApp a' e1' e2'
    AST.EOr a e1 e2     -> let a'  = desugarType a
                               e1' = desugarExp m e1
                               e2' = desugarExp m e2
                           in SEOr a' e1' e2'
    AST.EAnd a e1 e2    -> let a'  = desugarType a
                               e1' = desugarExp m e1
                               e2' = desugarExp m e2
                           in SEAnd a' e1' e2'
    AST.ERel a e1 op e2 -> let a'  = desugarType a
                               e1' = desugarExp m e1
                               e2' = desugarExp m e2
                               op' = fmap desugarType op
                           in SERel a' e1' op' e2'
    AST.EAdd a e1 op e2 -> let a'  = desugarType a
                               e1' = desugarExp m e1
                               e2' = desugarExp m e2
                               op' = fmap desugarType op
                           in SEAdd a' e1' op' e2'
    AST.EMul a e1 op e2 -> let a'  = desugarType a
                               e1' = desugarExp m e1
                               e2' = desugarExp m e2
                               op' = fmap desugarType op
                           in SEMul a' e1' op' e2'
    -- n-ary tuples become 2-ary
    AST.ETup a texps    -> case texps of
        [x]      -> desugarExp m x
        (x:y:xs) -> let (AST.TTup (_:as)) = a
                        a'                = AST.TTup as
                        texps'            = AST.ETup a' (y:xs)
                    in SETup (desugarType a) (desugarExp m x) (desugarExp m texps')
    AST.ENot a e'       -> SENot (desugarType a) (desugarExp m e')
    AST.EVar a id       -> SEVar (desugarType a) id
    AST.EUVar a uid     -> case Map.lookup uid m of
                                  Just id -> SEVar (desugarType a) id
                                  Nothing -> error "should not end up here - constructor not found"
    AST.EConst a c      -> SEConst (desugarType a) c

desugarPat :: AST.Pat AST.Type -> SPat SType
desugarPat p = case p of
    AST.PConst a c       -> SPConst (desugarType a) c
    AST.PVar a id        -> SPVar (desugarType a) id
    AST.PZAdt a uid      -> SPNAdt (desugarType a) uid Nothing
    AST.PNAdt a uid pats -> let p' = case pats of
                                    []  -> Nothing
                                    [x] -> Just $ desugarPat x
                                    _   -> let a' = AST.TTup (map getPatVar pats)
                                           in Just $ desugarPat $ AST.PTup a' pats
                            in SPNAdt (desugarType a) uid p'
    AST.PWild a          -> SPWild (desugarType a)
    AST.PNil a           -> SPNil (desugarType a)
    AST.PTup a pats      -> case pats of
        [x]      -> desugarPat x
        (x:y:xs) -> let (AST.TTup (_:as)) = a
                        a'                = AST.TTup as
                        pats'             = AST.PTup a' (y:xs)
                    in SPTup (desugarType a) (desugarPat x) (desugarPat pats')
    AST.PLay a id p'     -> SPLay (desugarType a) id (desugarPat p')

desugarPM :: Map.Map AST.UIdent AST.Ident -> AST.PatMatch AST.Type -> SPatMatch SType
desugarPM m (AST.PM p e) = SPM (desugarPat p) (desugarExp m e)

desugarType :: AST.Type -> SType
desugarType t = case t of
    AST.TLam t1 t2    -> STLam (desugarType t1) (desugarType t2)
    AST.TVar id       -> error "monomorphisation failed - found a type variable"
    AST.TNil          -> STNil
    AST.TAdt uid typs -> case typs of
        [] -> STAdt uid
        _  -> error "monomorphisation failed - found ADT with type variables"
    AST.TTup typs     -> case typs of
        [x]      -> desugarType x
        (x:y:xs) -> STTup (desugarType x) (desugarType (AST.TTup (y:xs)))
    AST.TBool         -> STBool
    AST.TInt          -> STInt
    AST.TFloat        -> STFloat







-- This is a copy of desugarFunction except instead of returning
-- `let p = e1 in`, it only returns the `e1` so the type is
-- `DS (SExp SType)` instead of `DS (SExp SType -> SExp SType)`
desugarFunction' :: [AST.Def AST.Type] -> DS (SExp SType)
desugarFunction' defs = do
    m <- gets constructorfuncs
    if numargs defs > 0
        then do
            -- new arguments for the function
            args' <- replicateM (numargs defs) fresh

            -- fetch the types the arguments should have
            let argtyps   = let (AST.DEquation _ _ args _) = head defs' in map getPatVar args
            -- create typed variables out of the generated variable names and the fetched types
            let typedargs = zipWith (\id typ -> AST.EVar typ id) args' argtyps
            -- create a tuple out of the new typed variables and simplify it
            let argtup    = desugarExp m (AST.ETup (AST.TTup argtyps) typedargs)

            -- case branches representing the different equations
            let casebranches = map (uncurry SPM) (branches m defs)
            -- result type of case expression
            let casetype     = getSExpVar (snd (head (branches m defs)))
            -- the actual case expression
            let caseExp      = SECase casetype argtup casebranches
            -- turn all the variables into patterns and bind them in lambdas
            let stypedargs   = map (\(AST.EVar t v) -> SPVar (desugarType t) v) typedargs
            let resultExp    = foldl (\body var@(SPVar a _) -> 
                                        SELam (STLam a (getSExpVar body)) var body)
                                    caseExp
                                    (reverse stypedargs)

            -- type and name of function to simplify
            let (typ, name) = typeAndName
            -- return either the recursive-tagged one or a normal let binding
            return $ resultExp
        else do let (typ, name)  = typeAndName
                let casebranches = branches m defs
                let let_ = case recursive defs of
                             True -> SELetR
                             False -> SELet
                case casebranches of
                    [(_,x)] -> return $ x
                    _   -> error "we should not end up here"
  where
      -- We don't care about the type signature any more, so we drop it if there is one.
      defs' :: [AST.Def AST.Type]
      defs' = case head defs of
          AST.DEquation _ _ _ _ -> defs
          AST.DTypeSig _ _      -> tail defs
    
      -- Name of the function
      typeAndName :: (SType, AST.Ident)
      typeAndName = case head defs of
          AST.DTypeSig id t      -> (desugarType t, id)
          AST.DEquation t id _ _ -> (desugarType t, id)

      -- A list of pairs of (patterns, body) representing an equation such as f patterns = body
      branches :: Map.Map AST.UIdent AST.Ident -> [AST.Def AST.Type] -> [(SPat SType, SExp SType)]
      branches m []                                = []
      branches m (AST.DTypeSig _ _: ds)            = branches m ds
      branches m (AST.DEquation t id args body:ds) =
          {- We are going to case match on the arguments, so we turn them into
          a long tuple instead (if there are more than one). -}
          let argtup = case length args of
                         1 -> head args
                         _ -> AST.PTup (AST.TTup (map getPatVar args)) args
              -- turn the tuple into a simplified tuple
              args'  = desugarPat argtup
              -- simplify the body
              body'  = desugarExp m body
          in (args', body') : branches m ds

      numargs :: [AST.Def AST.Type] -> Int
      numargs []                           = error "no definitions - should not end up here"
      numargs (AST.DTypeSig _ _:ds)        = numargs ds
      numargs (AST.DEquation _ _ args _:_) = length args
