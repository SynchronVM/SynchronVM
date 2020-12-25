module LambdaLifting.LambdaLifting (lambdaLift, renameExp) where

import Parser.AbsTinyCamiot
import Parser.PrintTinyCamiot
import Typechecker.AstUtils

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List

import System.IO.Unsafe (unsafePerformIO)

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

type LL b a = StateT Int (               -- ^ State to generate names
              Reader [(Ident, Type)]) a  -- ^ Variables currently in scope

lambdaLift :: [Def Type] -> Int -> ([Def Type], Int)
lambdaLift defs = runLL (llDefs defs)

{- | Runs the lifting computation and returns a pair with the new AST and
the lifted functions. -}
runLL :: LL Type [Def Type] -> Int -> ([Def Type], Int)
runLL ma state = do
    let rd = runStateT ma state
    runReader rd []
    
-- | Generate fresh function name
fresh :: LL a Ident
fresh = do
    i <- get
    put $ i + 1
    return $ Ident $ "v" ++ show i

-- | Lambda lift a program
llDefs :: [Def Type] -> LL Type [Def Type]
llDefs defs = do
    functions' <- mapM llFunction functions
    return $ foldl (\output (def, newdefs) -> output ++ newdefs ++ def) [] functions'
  where
    f (DEquation _ n1 _ _) (DEquation _ n2 _ _) = n1 == n2
    f (DTypeSig n1 _) (DEquation _ n2 _ _)      = n1 == n2
    f _ _                                       = False

    functions = groupBy f defs

-- | Lambda lift a function
llFunction :: [Def Type] -> LL Type ([Def Type], [Def Type])
llFunction []     = return ([], [])
llFunction (d:ds) = do
    (d', lifted)   <- llDef d
    (ds', lifted') <- llFunction ds
    return (d' : ds', lifted ++ lifted')

-- | Lambda lift a single definition
llDef :: Def Type -> LL Type (Def Type, [Def Type])
llDef d  = case d of
        DEquation a id ps e   -> do
            let pvars = concat $ map varsInPat ps
            local (myUnion pvars) $ do
                (e',ds) <- llExp e
                return (DEquation a id ps e', ds)
        DTypeSig _ _   -> return (d, [])
        DDataDec _ _ _ -> return (d, [])

-- | Lambda lift an expression
llExp :: Exp Type -> LL Type (Exp Type, [Def Type])
llExp e = case e of
        ECase a e pms   -> do
            (e',ds) <- llExp e
            pairs <- mapM llPatMatch pms
            let (pms',ds') = unzip pairs
            return (ECase a e' pms', ds ++ concat ds')
        ELet a p e1 e2  -> do
            let pvars = varsInPat p
            local (myUnion pvars) $ do
                (e1', ds1) <- llExp e1
                (e2', ds2) <- llExp e2
                return (ELet a p e1' e2', ds1++ds2)
        ELetR a p e1 e2 -> undefined
        ELam a p e      -> do
            let pvars      = varsInPat p -- vars in p
            (penv, pvars') <- convertPat p -- rename variables in p, returning an env and the new p

            -- recursively lambdalift the expression
            (e', d) <- local (`myUnion` pvars) (llExp e)

            -- calculate which of the bound variables need to be applied to the lifted function
            bound      <- ask
            let evars   = varsInExp e'                 -- vars in e
            let unbound = exclude evars pvars          -- vars in e that are not in p
            let toApply = myIntersection bound unbound -- apply those that are bound

            -- fresh names for the applied variables in the lifted expression
            toApply' <- mapM (\(_,t) -> fresh >>= \v -> return (v,t)) toApply

            -- environment for renaming of the expression
            let env  = Map.fromList $ zipWith (\x y -> (fst x, fst y)) toApply toApply'
            let m    = Map.union env penv
            let e''  = renameExp m e'
            
            -- All arguments the lifted function will have, and the type of the lifted function
            let args = map (\(v,t) -> (PVar t v, t)) toApply' ++ [(pvars', getPatVar pvars')]
            let ftype = foldr (TLam . snd) (unwrapFunction a) args

            -- Create the lifted function
            f          <- fresh              -- fresh name for the lifted function
            let dtypsig = DTypeSig f ftype   -- it's type signature
            let def     = DEquation ftype f (map fst args) e''  -- the equation itself

            -- Create the new application
            let res = foldl (\f' (v,t') -> case f' of
                                     (EVar (TLam _ t) _)   -> EApp t f' (EVar t' v)
                                     (EApp (TLam _ t) _ _) -> EApp t f' (EVar t' v))
                            (EVar ftype f)
                            toApply

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
varsInExp :: Exp Type -> [(Ident, Type)]
varsInExp e = case e of
        ECase _ e pms   -> let s = varsInExp e
                               varsInPm (PM p e') = exclude (varsInExp e') (varsInPat p)
                           in concat $ s : map varsInPm pms
        ELet _ p e1 e2  -> let s1 = varsInExp e1
                               s2 = varsInExp e2
                               pi = varsInPat p
                           in exclude s1 pi ++ exclude s2 pi
        ELetR a p e1 e2 -> undefined
        ELam _ p e      -> exclude (varsInExp e) (varsInPat p)
        EIf _ e1 e2 e3  -> varsInExp e1 ++ varsInExp e2 ++ varsInExp e3
        EApp _ e1 e2    -> varsInExp e1 ++ varsInExp e2
        EOr _ e1 e2     -> varsInExp e1 ++ varsInExp e2
        EAnd _ e1 e2    -> varsInExp e1 ++ varsInExp e2
        ERel _ e1 _ e2  -> varsInExp e1 ++ varsInExp e2
        EAdd _ e1 _ e2  -> varsInExp e1 ++ varsInExp e2
        EMul _ e1 _ e2  -> varsInExp e1 ++ varsInExp e2
        ETup _ texps    -> concat $ map varsInExp texps
        ENot _ e        -> varsInExp e
        EVar a id       -> [(id, a)]
        EUVar _ _       -> []
        EConst _ _      -> []

-- | Some set operations on lists (lol cheeky)
exclude :: (Ord a, Ord b) => [(a,b)] -> [(a,b)] -> [(a,b)]
exclude xs ys = Set.toList $ Set.fromList xs Set.\\ Set.fromList ys

myUnion :: (Ord a, Ord b) => [(a,b)] -> [(a,b)] -> [(a,b)]
myUnion xs ys = Set.toList $ Set.union (Set.fromList xs) (Set.fromList ys)

myIntersection :: (Ord a, Ord b) => [(a,b)] -> [(a,b)] -> [(a,b)]
myIntersection xs ys = Set.toList $ Set.intersection (Set.fromList xs) (Set.fromList ys)

varsInPat :: Pat Type -> [(Ident, Type)]
varsInPat p = case p of
        PConst _ _      -> []
        PVar a id       -> [(id, a)]
        PZAdt _ _       -> []
        PNAdt _ _ apats -> concat $ map varsInPat apats
        PWild a         -> []
        PNil a          -> []
        PTup _ tpats    -> concat $ map varsInPat tpats
        PLay a id p'    -> (id, a) :  varsInPat p'

convertPat :: Pat Type -> LL Type (Map.Map Ident Ident, Pat Type)
convertPat p = case p of
        PConst _ _        -> return (Map.empty, p)
        PVar a id         -> do id' <- fresh
                                return (Map.singleton id id', PVar a id')
        PZAdt _ _         -> return (Map.empty, p)
        PNAdt a uid apats -> do (maps, apats') <- unzip <$> mapM convertPat apats
                                let m = Map.unions maps
                                return (m, PNAdt a uid apats')
        PWild _           -> return (Map.empty, p)
        PNil _            -> return (Map.empty, p)
        PTup a tpats      -> do (maps, tpats') <- unzip <$> mapM convertPat tpats
                                let m = Map.unions maps
                                return (m, PTup a tpats')
        PLay a id p'      -> do id' <- fresh
                                (m, p'') <- convertPat p'
                                let m' = Map.union m (Map.singleton id id')
                                return (m', PLay a id' p'')

-- | Lambda lifts a case-branch
llPatMatch :: PatMatch Type -> LL Type (PatMatch Type, [Def Type])
llPatMatch (PM p e) = do
    let pvars = varsInPat p
    local (myUnion pvars) $ do
        (e',ds) <- llExp e
        return (PM p e', ds)

{- | Takes an environment and an expression, and renames any variables that are
present in the environment. -}
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