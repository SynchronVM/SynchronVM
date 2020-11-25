module Renaming.Renaming (rename) where

import Parser.AbsTinyCamiot ( Def(..), Exp(..), Ident(..), Pat(..), PatMatch(..) )
import Parser.PrintTinyCamiot ( printTree )

import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask, local) )
import Control.Monad.State  (StateT(runStateT), MonadState(put, get) )
import qualified Data.Map as Map

-- | Internal state used to generate fresh names
type StateEnv    = Int

-- | The reader state will map old names to new, fresh names
type ReaderState = Map.Map Ident Ident

-- | The renaming monad
type R a = StateT StateEnv (
             ReaderT ReaderState IO) a

-- | Run a renaming computation
runR :: R a -> IO a
runR ra = do
    let rea = runStateT ra 0
    (a,_) <- runReaderT rea Map.empty
    return a

-- | Alpha-rename a program
rename :: [Def a ] -> IO [Def a]
rename ds = runR $ renameDef ds

-- | Generate a fresh name
fresh :: R Ident
fresh = do
    i <- get
    put (i + 1)
    return $ Ident ("var" ++ show i)

{- | Extend the local environment with the new (old, new)-name pair, and run the
renaming compuation in the second argument. -}
inEnv :: (Ident, Ident) -> R a -> R a
inEnv (idfrom, idto) = local (Map.insert idfrom idto)

-- | Same as above, but adds many (old, new)-name pairs at once.
-- Important to use the left-biased union. since we want to, in the local computation,
-- keep mainly the names in names.
inEnvMany :: [(Ident, Ident)] -> R a -> R a
inEnvMany names = local (Map.union (Map.fromList names))

-- | Returns `True` if there exists a name for this variable in the local environment.
isRenamed :: Ident -> R Bool
isRenamed id = do
    e <- ask
    case Map.lookup id e of
        Just _  -> return True
        Nothing -> return False

-- | Rename a list of definitions.
renameDef :: [Def a] -> R [Def a]
renameDef []     = return []
renameDef (d:ds) = case d of
    DEquation _ id _ _ -> do
        b <- isRenamed id
        if b
            then do d'  <- renameDef' d
                    ds' <- renameDef ds
                    return $ d':ds'
            else do id' <- fresh
                    inEnv (id, id') (renameDef (d:ds))
    DTypeSig id t       -> do
        id' <- fresh
        inEnv (id, id') (renameDef ds >>= \ds' -> return $ DTypeSig id' t : ds')
    DDataDec uid ids cd -> renameDef ds >>= \ds' -> return $ DDataDec uid ids cd : ds'
  where
        renameDef' :: Def a -> R (Def a)
        renameDef' (DEquation a id ps e) = do
            env <- ask
            case Map.lookup id env of
                Just id' -> do (ps',e') <- renamePat ps (renameExp e)
                               return $ DEquation a id' ps' e'
                Nothing  -> undefined

-- | Rename a case-match branch.
renamePatMatch :: PatMatch a -> R (PatMatch a)
renamePatMatch (PM p e) = renamePat [p] (renameExp e) >>= \([p'],e') -> return $ PM p' e'

-- | Rename an expression.
renameExp :: Exp a -> R (Exp a)
renameExp e = case e of
    ECase a e pms   -> do
        e'   <- renameExp e
        pms' <- mapM renamePatMatch pms
        return $ ECase a e' pms'
    ELet a p e1 e2  -> do
        ([p'], [e1', e2']) <- renamePat [p] (sequence [renameExp e1, renameExp e2])
        return $ ELet a p' e1' e2'
    ELetR a p e1 e2 -> undefined
    ELam a p e      -> do
        ([p'], e') <- renamePat [p] (renameExp e)
        return $ ELam a p' e'
    EIf a e1 e2 e3  -> do
        e1' <- renameExp e1
        e2' <- renameExp e2
        e3' <- renameExp e3
        return $ EIf a e1' e2' e3'
    EApp a e1 e2    -> do
        e1' <- renameExp e1
        e2' <- renameExp e2
        return $ EApp a e1' e2'
    EOr a e1 e2     -> do
        e1' <- renameExp e1
        e2' <- renameExp e2
        return $ EOr a e1' e2'
    EAnd a e1 e2    -> do
        e1' <- renameExp e1
        e2' <- renameExp e2
        return $ EAnd a e1' e2'
    ERel a e1 op e2 -> do
        e1' <- renameExp e1
        e2' <- renameExp e2
        return $ ERel a e1' op e2'
    EAdd a e1 op e2 -> do
        e1' <- renameExp e1
        e2' <- renameExp e2
        return $ EAdd a e1' op e2'
    EMul a e1 op e2 -> do
        e1' <- renameExp e1
        e2' <- renameExp e2
        return $ EMul a e1' op e2'
    ETup a tes      -> mapM renameExp tes >>= \tes' -> return $ ETup a tes'
    ENot a e        -> renameExp e  >>= \e'  -> return $ ENot a e'
    EVar a id       -> renameVar id >>= \id' -> return $ EVar a id'
    EUVar a uid     -> return $ EUVar a uid
    EConst a c      -> return $ EConst a c

-- | Rename a variable. If the variable has no new name an error is thrown.
renameVar :: Ident -> R Ident
renameVar id = do
    e <- ask
    case Map.lookup id e of
        Just id' -> return id'
        Nothing  -> error $ "name " ++ printTree id ++ " not found in environment"

-- | Rename a pattern.
renamePat :: [Pat a] -> R b -> R ([Pat a], b)
renamePat ps mb = do
    (ps', names) <- unzip <$> mapM giveFreshName ps
    b <- inEnvMany (concat names) mb
    return (ps', b)
  where {-- | Renames a pattern and returns the new pattern and a list of
              old-new name pairs. -}
        giveFreshName :: Pat a -> R (Pat a, [(Ident, Ident)])
        giveFreshName p = case p of
            PConst a c          -> return (PConst a c, [])
            PVar a id           -> do
                id' <- fresh
                return (PVar a id', [(id, id')])
            PZAdt a uid         -> return (PZAdt a uid, [])
            PNAdt a uid adtpats -> do
                a' <- mapM giveFreshName adtpats
                let adtpats' = map fst a'
                let names    = concatMap snd a'
                return (PNAdt a uid adtpats', names)
            PWild a             -> return (PWild a, [])
            PNil a              -> return (PNil a, [])
            PTup a tpats        -> do
                a' <- mapM giveFreshName tpats
                let tpats' = map fst a'
                let names  = concatMap snd a'
                return (PTup a tpats', names)
            PLay a id p         -> do
                id' <- fresh
                (p', names') <- giveFreshName p
                return (PLay a id' p', (id, id') : names')