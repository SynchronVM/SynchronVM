module LambdaLifting.LambdaLifting (lambdaLift) where

import Parser.AbsTinyCamiot

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Set as Set


type LL b a = WriterT [Def b] (           -- ^ Emit new functions
              StateT Int (                -- ^ State to generate names
              Reader (Set.Set Ident))) a  -- ^ Variables currently in scope

lambdaLift :: [Def b] -> IO [Def b]
lambdaLift defs = runLL (llDefs defs) --do
    --(lifted, defs') <- runLL (llDefs defs)
    --return $ lifted ++ defs'

{- | Runs the lifting computation and returns a pair with the new AST and
the lifted functions. -}
runLL :: LL a [Def a] -> IO [Def a] -- ([Def b], [Def b])
runLL ma = do
    let st = runWriterT ma
    let rd = evalStateT st 0
    let (defs, _) = runReader rd Set.empty
    return defs --return (lifted, defs)

-- | Generate fresh function name
fresh :: LL a Ident
fresh = do
    i <- get
    put $ i + 1
    return $ Ident $ "lvar" ++ show i

llDefs :: [Def a] -> LL a [Def a]
llDefs []     = return []
llDefs (d:ds) = do
    d' <- llDef d
    ds' <- llDefs ds
    return $ d' ++ ds'

llDef :: Def a -> LL a [Def a]
llDef d  = case d of
        DEquation a id ps e   -> do
            let pvars = Set.unions $ map varsInPat ps
            local (Set.union pvars) $ do
                (e',ds) <- llExp e
                return $ ds ++ [DEquation a id ps e']
        DTypeSig id t         -> return [d]
        DDataDec uid id cdecs -> return [d]

llExp :: Exp a -> LL a (Exp a, [Def a])
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
            -- TODO currently only handles 1 level of nesting
            let pvars = varsInPat p -- vars in p
            let evars = varsInExp e -- vars in e
            let unbound = evars Set.\\ pvars -- vars in e that are not in p
            
            -- previously bound variables
            bound <- ask
            -- if any of them are used in e, we need to apply them
            let toApply = Set.intersection bound unbound

            -- fresh name for the function
            f <- fresh

            -- build the resulting expression
            let res  = foldl (EApp a) (EVar a f) (map (EVar a) (Set.toList toApply))
            let args = Set.toList $ Set.union toApply pvars
            let def  = DEquation a f (map (PVar a) args) e
            tell [def]
            return (res, [def])
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
varsInExp :: Exp a -> Set.Set Ident
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
        EVar _ id       -> Set.singleton id
        EUVar _ _       -> Set.empty
        EConst _ _      -> Set.empty

varsInPat :: Pat a -> Set.Set Ident
varsInPat p = case p of
        PConst _ _      -> Set.empty
        PVar _ id       -> Set.singleton id
        PZAdt _ _       -> Set.empty
        PNAdt _ _ apats -> Set.unions (map varsInPat apats)
        PWild _         -> Set.empty
        PNil _          -> Set.empty
        PTup _ tpats    -> Set.unions (map varsInPat tpats)
        PLay _ id p'    -> Set.union (Set.singleton id) (varsInPat p')

llPatMatch :: PatMatch a -> LL a (PatMatch a, [Def a])
llPatMatch (PM p e) = do
    let pvars = varsInPat p
    local (Set.union pvars) $ do
        (e',ds) <- llExp e
        return (PM p e', ds)