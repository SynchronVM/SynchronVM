module Renaming.Renaming where

import Parser.AbsTinyCamiot
import Parser.PrintTinyCamiot

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map

type StateEnv    = Int
type ReaderState = Map.Map Ident Ident
type R a = StateT StateEnv (
             ReaderT ReaderState IO) a

fresh :: R Ident
fresh = do
    i <- get
    put (i + 1)
    return $ Ident ("var" ++ show i)

inEnv :: (Ident, Ident) -> R a -> R a
inEnv (idfrom, idto) = local (Map.insert idfrom idto)

-- Important to use the left-biased union. since we want to, in the local computation,
-- keep mainly the names in names.
inEnvMany :: [(Ident, Ident)] -> R a -> R a
inEnvMany names = local (Map.union (Map.fromList names))

renameDef :: [Def a] -> R [Def a]
renameDef (d:ds) = case d of
    DEquation a id ps e    -> undefined
    DTypeSig id t          -> undefined
    DDataDec uid ids cdecs -> undefined

renamePatMatch :: PatMatch a -> R (PatMatch a)
renamePatMatch (PM p e) = undefined

renameExp :: Exp a -> R (Exp a)
renameExp e = case e of
    ECase a e pms   -> undefined
    ELet a p e1 e2  -> undefined
    ELetR a p e1 e2 -> undefined
    ELam a p e      -> undefined
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

renameVar :: Ident -> R Ident
renameVar id = do
    e <- ask
    case Map.lookup id e of
        Just id' -> return id'
        Nothing  -> error $ "name " ++ printTree id ++ " not found in environment"

renamePat :: Pat a -> R b -> R (Pat a, b)
renamePat p mb = do
    (p', names) <- giveFreshName p
    b <- inEnvMany names mb
    return (p', b)
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