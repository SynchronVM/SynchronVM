module LambdaLifting.LambdaLifting (lambdaLift) where

import Parser.AbsTinyCamiot
import Parser.PrintTinyCamiot
import Typechecker.AstUtils

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List

type LL b a = StateT Int (                           -- ^ State to generate names
              ReaderT (Set.Set (Ident, Type)) IO) a  -- ^ Variables currently in scope

lambdaLift :: [Def Type] -> Int -> IO [Def Type]
lambdaLift defs state = runLL (llDefs defs) state --do
    --(lifted, defs') <- runLL (llDefs defs)
    --return $ lifted ++ defs'

{- | Runs the lifting computation and returns a pair with the new AST and
the lifted functions. -}
runLL :: LL Type [Def Type] -> Int -> IO [Def Type] -- ([Def b], [Def b])
runLL ma state = do
    let rd = evalStateT ma state
    runReaderT rd Set.empty
    --return defs --return (lifted, defs)

-- | Generate fresh function name
fresh :: LL a Ident
fresh = do
    i <- get
    put $ i + 1
    return $ Ident $ "v" ++ show i

llDefs :: [Def Type] -> LL Type [Def Type]
llDefs defs = do
    functions' <- mapM llFunction functions
    return $ foldl (\output (def, newdefs) -> output ++ newdefs ++ def) [] functions'
  where
    f (DEquation _ n1 _ _) (DEquation _ n2 _ _) = n1 == n2
    f (DTypeSig n1 _) (DEquation _ n2 _ _)      = n1 == n2
    f _ _                                       = False

    functions = groupBy f defs

llFunction :: [Def Type] -> LL Type ([Def Type], [Def Type])
llFunction []     = return ([], [])
llFunction (d:ds) = do
    (d', lifted)   <- llDef d
    (ds', lifted') <- llFunction ds
    return (d' : ds', lifted ++ lifted')

llDef :: Def Type -> LL Type (Def Type, [Def Type])--[Def a]
llDef d  = case d of
        DEquation a id ps e   -> do
            let pvars = Set.unions $ map varsInPat ps
            local (Set.union pvars) $ do
                (e',ds) <- llExp e
                return (DEquation a id ps e', ds)
        DTypeSig _ _   -> return (d, [])
        DDataDec _ _ _ -> return (d, [])

llExp :: Exp Type -> LL Type (Exp Type, [Def Type])
llExp e = case e of
        ECase a e pms   -> do
            (e',ds) <- llExp e
            pairs <- mapM llPatMatch pms
            let (pms',ds') = unzip pairs
            return (ECase a e pms', ds ++ concat ds')
        ELet a p e1 e2  -> do
            let pvars = varsInPat p
            local (Set.union pvars) $ do
                (e1', ds1) <- llExp e1
                (e2', ds2) <- llExp e2
                return (ELet a p e1' e2', ds1++ds2)
        ELetR a p e1 e2 -> undefined
        ELam a p e      -> do
            let pvars = varsInPat p -- vars in p

            -- first, recursively lambdalift the expression to lift
            (e', d) <- local (`Set.union` pvars) (llExp e)

            -- calculate which of the bound variables need to be applied to the lifted function
            bound      <- ask
            let evars   = varsInExp e' -- vars in e
            let unbound = evars Set.\\ pvars -- vars in e that are not in p
            let toApply = Set.intersection bound unbound -- if any of them are used in e, we need to apply them

            -- fresh names for the applied variables in the lifted expression
            toApply' <- Set.fromList <$> mapM (\(_,t) -> do v <- fresh
                                                            return (v,t)) (Set.toList toApply)

            -- environment for renaming of the expression
            let env = Map.fromList $ zipWith (\x y -> (fst x, fst y)) (Set.toList toApply) (Set.toList toApply')
            let e'' = renameExp env e'

            -- fresh name for the function we are lifting
            f <- fresh
            -- vars to apply lifted function to
            let args2@((_,t):_) = Set.toList toApply ++  Set.toList pvars
            let ftype = foldl (\t' (_,t'') -> TLam t' t'') t (tail args2 ++ [(undefined, unwrapFunction a)])
            
            let dtypsig = DTypeSig f ftype
            let def     = DEquation ftype f (map (\(v,t) -> PVar t v) args2) e''

            let res = foldl (\f'@(EVar (TLam _ t) _) (v,t') -> EApp t f' (EVar t' v) ) (EVar ftype f) (Set.toList toApply)

            return (res, d ++ [dtypsig, def])
        EIf a e1 e2 e3  -> do
            (e1',ds1) <- llExp e1
            (e2',ds2) <- llExp e2
            (e3',ds3) <- llExp e3
            return (EIf a e1' e2' e3', ds1++ds2++ds3)
        EApp a e1 e2    -> do
            (e1',ds1) <- llExp e1
            (e2',ds2) <- llExp e2
            return (EApp a e1' e2', ds1++ds2)
        EOr a e1 e2     -> do
            (e1',ds1) <- llExp e1
            (e2',ds2) <- llExp e2
            return (EOr a e1' e2', ds1++ds2)
        EAnd a e1 e2    -> do
            (e1',ds1) <- llExp e1
            (e2',ds2) <- llExp e2
            return (EAnd a e1' e2', ds1++ds2)
        ERel a e1 op e2 -> do
            (e1',ds1) <- llExp e1
            (e2',ds2) <- llExp e2
            return (ERel a e1' op e2', ds1++ds2)
        EAdd a e1 op e2 -> do
            (e1',ds1) <- llExp e1
            (e2',ds2) <- llExp e2
            return (EAdd a e1' op e2', ds1++ds2)
        EMul a e1 op e2 -> do
            (e1',ds1) <- llExp e1
            (e2',ds2) <- llExp e2
            return (EMul a e1' op e2', ds1++ds2)
        ETup a texps    -> do
            pairs <- mapM llExp texps
            let (texps', ds) = unzip pairs
            return (ETup a texps', concat ds)
        ENot a e        -> llExp e >>= \(e',ds) -> return (ENot a e', ds)
        EVar a id       -> return (EVar a id, [])
        EUVar a uid     -> return (EUVar a uid, [])
        EConst a c      -> return (EConst a c, [])

{- | Returns the set of unbound variables in an expression. E.g \x -> x + y would return
the set containing only y, as we clearly see where x is bound. -}
varsInExp :: Exp Type -> Set.Set (Ident, Type)
varsInExp e = case e of
        ECase _ e pms   -> let s = varsInExp e
                               varsInPm (PM p e') = varsInExp e' Set.\\ varsInPat p
                           in Set.unions $ s : map varsInPm pms
        ELet _ p e1 e2  -> let s1 = varsInExp e1
                               s2 = varsInExp e2
                               pi = varsInPat p
                           in Set.union (s1 Set.\\ pi) (s2 Set.\\ pi)
        ELetR a p e1 e2 -> undefined
        ELam _ p e      -> varsInExp e Set.\\ varsInPat p
        EIf _ e1 e2 e3  -> Set.unions [varsInExp e1, varsInExp e2, varsInExp e3]
        EApp _ e1 e2    -> Set.union (varsInExp e1) (varsInExp e2)
        EOr _ e1 e2     -> Set.union (varsInExp e1) (varsInExp e2)
        EAnd _ e1 e2    -> Set.union (varsInExp e1) (varsInExp e2)
        ERel _ e1 _ e2  -> Set.union (varsInExp e1) (varsInExp e2)
        EAdd _ e1 _ e2  -> Set.union (varsInExp e1) (varsInExp e2)
        EMul _ e1 _ e2  -> Set.union (varsInExp e1) (varsInExp e2)
        ETup _ texps    -> Set.unions (map varsInExp texps)
        ENot _ e        -> varsInExp e
        EVar a id       -> Set.singleton (id, a)
        EUVar _ _       -> Set.empty
        EConst _ _      -> Set.empty

varsInPat :: Pat Type -> Set.Set (Ident, Type)
varsInPat p = case p of
        PConst _ _      -> Set.empty
        PVar a id       -> Set.singleton (id, a)
        PZAdt _ _       -> Set.empty
        PNAdt _ _ apats -> Set.unions (map varsInPat apats)
        PWild _         -> Set.empty
        PNil _          -> Set.empty
        PTup _ tpats    -> Set.unions (map varsInPat tpats)
        PLay a id p'    -> Set.union (Set.singleton (id, a)) (varsInPat p')

llPatMatch :: PatMatch Type -> LL Type (PatMatch Type, [Def Type])
llPatMatch (PM p e) = do
    let pvars = varsInPat p
    local (Set.union pvars) $ do
        (e',ds) <- llExp e
        return (PM p e', ds)

renameExp :: Map.Map Ident Ident -> Exp Type -> Exp Type
renameExp env e = case e of
    ECase a e pms   -> let pms' = map (\(PM p' e') -> PM p' (renameExp env e')) pms
                       in ECase a (renameExp env e) pms'
    ELet a p e1 e2  -> let e1' = renameExp env e1
                           e2' = renameExp env e2
                       in ELet a p e1' e2'
    ELetR a p e1 e2 -> undefined
    ELam a p e      -> ELam a p (renameExp env e)
    EIf a e1 e2 e3  -> let e1' = renameExp env e1
                           e2' = renameExp env e2
                           e3' = renameExp env e3
                       in EIf a e1' e2' e3'
    EApp a e1 e2    -> let e1' = renameExp env e1
                           e2' = renameExp env e2
                       in EApp a e1' e2'
    EOr a e1 e2     -> let e1' = renameExp env e1
                           e2' = renameExp env e2
                       in EOr a e1' e2'
    EAnd a e1 e2    -> let e1' = renameExp env e1
                           e2' = renameExp env e2
                       in EAnd a e1' e2'
    ERel a e1 o e2  -> let e1' = renameExp env e1
                           e2' = renameExp env e2
                       in ERel a e1' o e2'
    EAdd a e1 o e2  -> let e1' = renameExp env e1
                           e2' = renameExp env e2
                       in EAdd a e1' o e2'
    EMul a e1 o e2  -> let e1' = renameExp env e1
                           e2' = renameExp env e2
                       in EMul a e1' o e2'
    ETup a texps    -> ETup a $ map (renameExp env) texps
    ENot a e        -> ENot a (renameExp env e)
    EVar a id       -> case Map.lookup id env of
                        Just id' -> EVar a id'
                        Nothing  -> EVar a id
    EUVar _ _       -> e
    EConst _ _      -> e