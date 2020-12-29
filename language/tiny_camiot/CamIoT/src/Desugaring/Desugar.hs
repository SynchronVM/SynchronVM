module Desugaring.Desugar where

import Typechecker.AstUtils
import qualified Parser.AbsTinyCamiot as AST
import Control.Monad.State
import Data.Foldable
import Data.Maybe
import Desugaring.AST

import System.IO.Unsafe

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

{- ********** Actual desugaring ********** -}

type DS a = State Int a

desugar :: Int -> [AST.Def AST.Type] -> SExp SType
desugar state defs = runDS state $ desugarFunctions funs' (fromJust main)
  where
      funs  = groupAsFunctions defs
      funs' = filter (\(d:_) -> case d of
                    AST.DDataDec _ _ _     -> False
                    AST.DTypeSig id _      -> id /= AST.Ident "main"
                    AST.DEquation _ id _ _ -> id /= AST.Ident "main") funs
      main  = find (\(d:_) -> case d of
                    AST.DDataDec _ _ _     -> False
                    AST.DTypeSig id _      -> id == AST.Ident "main"
                    AST.DEquation _ id _ _ -> id == AST.Ident "main")
                  funs

runDS :: Int -> DS a -> a
runDS state ds = evalState ds state

fresh :: DS AST.Ident
fresh = do
    st <- get
    put (st + 1)
    return $ AST.Ident $ "v" ++ show st

desugarFunctions :: [[AST.Def AST.Type]] -> [AST.Def AST.Type] -> DS (SExp SType)
desugarFunctions (fun:funs) main = case funs of
    [] -> do fun' <- desugarFunction fun
             return $ fun' (desugarMain main)
    _  -> do fun' <- desugarFunction fun
             rest <- desugarFunctions funs main
             return $ fun' rest
  where
    desugarMain :: [AST.Def AST.Type] -> SExp SType
    desugarMain [AST.DEquation _ _ _ body] = desugarExp body
    desugarMain _                          = error "wrong form of main function"

-- | Desugar a single function, represented as a list of definitions.
desugarFunction :: [AST.Def AST.Type] -> DS (SExp SType -> SExp SType)
desugarFunction defs = do
    -- new arguments for the function
    args' <- replicateM (numargs defs) fresh

    -- fetch the types the arguments should have
    let argtyps   = let (AST.DEquation _ _ args _) = head defs' in map getPatVar args
    -- create typed variables out of the generated variable names and the fetched types
    let typedargs = zipWith (\id typ -> AST.EVar typ id) args' argtyps
    -- create a tuple out of the new typed variables and simplify it
    let argtup    = desugarExp (AST.ETup (AST.TTup argtyps) typedargs)

    -- case branches representing the different equations
    let casebranches = map (uncurry SPM) (branches defs)
    -- result type of case expression
    let casetype     = getSExpVar (snd (head (branches defs)))
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
      branches :: [AST.Def AST.Type] -> [(SPat SType, SExp SType)]
      branches []                                = []
      branches (AST.DTypeSig _ _: ds)            = branches ds
      branches (AST.DEquation t id args body:ds) =
          {- We are going to case match on the arguments, so we turn them into
          a long tuple instead (if there are more than one). -}
          let argtup = case length args of
                         1 -> head args
                         _ -> AST.PTup (AST.TTup (map getPatVar args)) args
              -- turn the tuple into a simplified tuple
              args'  = desugarPat argtup
              -- simplify the body
              body'  = desugarExp body
          in (args', body') : branches ds

      numargs :: [AST.Def AST.Type] -> Int
      numargs []                           = error "no definitions - should not end up here"
      numargs (AST.DTypeSig _ _:ds)        = numargs ds
      numargs (AST.DEquation _ _ args _:_) = length args

desugarExp :: AST.Exp AST.Type -> SExp SType
desugarExp e = case e of
    AST.ECase a e' pms  -> let a'   = desugarType a
                               e''  = desugarExp e'
                               pms' = map desugarPM pms
                           in SECase a' e'' pms'
    AST.ELet a p e1 e2  -> let a'  = desugarType a
                               p'  = desugarPat p
                               e1' = desugarExp e1
                               e2' = desugarExp e2
                           in SELet a' p' e1' e2'
    AST.ELetR a p e1 e2 -> let a'  = desugarType a
                               p'  = desugarPat p
                               e1' = desugarExp e1
                               e2' = desugarExp e2
                           in SELetR a' p' e1' e2'
    AST.ELam a p e'     -> let a'  = desugarType a
                               p'  = desugarPat p
                               e'' = desugarExp e'
                           in SELam a' p' e''
    -- If becomes a case expression
    AST.EIf a e1 e2 e3  -> let a'  = desugarType a
                               e1' = desugarExp e1
                               e2' = desugarExp e2
                               e3' = desugarExp e3
                           in SEIf a' e1' e2' e3'
    AST.EApp a e1 e2    -> let a'  = desugarType a
                               e1' = desugarExp e1
                               e2' = desugarExp e2
                           in SEApp a' e1' e2'
    AST.EOr a e1 e2     -> let a'  = desugarType a
                               e1' = desugarExp e1
                               e2' = desugarExp e2
                           in SEOr a' e1' e2'
    AST.EAnd a e1 e2    -> let a'  = desugarType a
                               e1' = desugarExp e1
                               e2' = desugarExp e2
                           in SEAnd a' e1' e2'
    AST.ERel a e1 op e2 -> let a'  = desugarType a
                               e1' = desugarExp e1
                               e2' = desugarExp e2
                               op' = fmap desugarType op
                           in SERel a' e1' op' e2'
    AST.EAdd a e1 op e2 -> let a'  = desugarType a
                               e1' = desugarExp e1
                               e2' = desugarExp e2
                               op' = fmap desugarType op
                           in SEAdd a' e1' op' e2'
    AST.EMul a e1 op e2 -> let a'  = desugarType a
                               e1' = desugarExp e1
                               e2' = desugarExp e2
                               op' = fmap desugarType op
                           in SEMul a' e1' op' e2'
    -- n-ary tuples become 2-ary
    AST.ETup a texps    -> case texps of
        [x]      -> desugarExp x
        (x:y:xs) -> let (AST.TTup (_:as)) = a
                        a'                = AST.TTup as
                        texps'            = AST.ETup a' (y:xs)
                    in SETup (desugarType a) (desugarExp x) (desugarExp texps')
    AST.ENot a e'       -> SENot (desugarType a) (desugarExp e')
    AST.EVar a id       -> SEVar (desugarType a) id
    AST.EUVar a uid     -> SEUVar (desugarType a) uid
    AST.EConst a c      -> SEConst (desugarType a) c

desugarPat :: AST.Pat AST.Type -> SPat SType
desugarPat p = case p of
    AST.PConst a c       -> SPConst (desugarType a) c
    AST.PVar a id        -> SPVar (desugarType a) id
    AST.PZAdt a uid      -> SPNAdt (desugarType a) uid []
    AST.PNAdt a uid pats -> SPNAdt (desugarType a) uid (map desugarPat pats)
    AST.PWild a          -> SPWild (desugarType a)
    AST.PNil a           -> SPNil (desugarType a)
    AST.PTup a pats      -> case pats of
        [x]      -> desugarPat x
        (x:y:xs) -> let (AST.TTup (_:as)) = a
                        a'                = AST.TTup as
                        pats'             = AST.PTup a' (y:xs)
                    in SPTup (desugarType a) (desugarPat x) (desugarPat pats')
    AST.PLay a id p'     -> SPLay (desugarType a) id (desugarPat p')

desugarPM :: AST.PatMatch AST.Type -> SPatMatch SType
desugarPM (AST.PM p e) = SPM (desugarPat p) (desugarExp e)

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