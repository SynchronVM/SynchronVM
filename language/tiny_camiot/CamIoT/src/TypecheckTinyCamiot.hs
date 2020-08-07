module TypecheckTinyCamiot where

import AbsTinyCamiot

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

{- ******************** -}
-- top level typecheck

typecheck :: [Def ()] -> IO (Either String [Def (Type ())])
typecheck defs = do
    res <- runExceptT (runStateT pgm emptyState)
    case res of
        Left e -> return $ Left (show e)         -- error occured
        Right (defs', _) -> return $ Right defs' -- TC completed
  where pgm = check defs -- the monadic computation that TC's

{- ******************** -}
-- typecheck monad

-- Data type representing all possible errors that can be raised
-- Add variants and needed and include a show instance for it, so that
-- it is rendered nicely.
data TCError = ERROR

instance Show TCError where

-- The state kept by the typechecker as it typechecks a program
type TCState = String

emptyState :: TCState
emptyState = ""

-- The typechecking monad! Please change as you see fit. Perhaps we don't
-- want IO in the bottom of it, but i find it usually helps me debug stuff,
-- as you can always just print whatever you want.
type TC a = StateT TCState (ExceptT TCError IO) a

{- ******************** -}
-- typecheck

check :: [Def ()] -> TC [Def (Type ())]
check = undefined

check_ :: Def () -> TC (Def (Type ()))
check_ d = case d of
    DEquation a id patterns body -> undefined
    DTypeSig a id t -> undefined
    DDataDec a name typevars constructors -> undefined

checkExp :: Exp () -> TC (Exp (Type ()))
checkExp e = case e of
    ETup _ tupExps -> do
        -- type the tuple components
        typedTupExps <- mapM (checkExp . deTupExp) tupExps
        -- fetch the types
        let types = map getExpvar typedTupExps
        -- create the tuple type
        let typ = undefined -- TPair etc etc
        -- return the typed tuple expression
        return $ ETup typ (map tupExp typedTupExps)

    ECase _ e1 patterns -> undefined
    ELet _ pattern e1 e2 -> undefined
    ELetR _ pattern e1 e2 -> undefined
    ELam _ pattern e1 -> undefined
    EIf _ e1 e2 e3 -> undefined
    ECon _ constructor exps -> undefined
    EApp _ e1 e2 -> undefined
    EOr _ e1 e2 -> undefined
    EAnd _ e1 e2 -> undefined
    ERel _ e1 op e2 -> undefined
    EAdd _ e1 op e2 -> undefined
    EMul _ e1 op e2 -> undefined
    ENot _ e1 -> undefined
    EVar _ var -> undefined
    
    EConst _ const -> return $ case const of
        CInt _ i   -> fmap (\_ -> go "Int") e
        CFloat _ d -> fmap (\_ -> go "Float") e
        CTrue _    -> fmap (\_ -> go "Bool") e
        CFalse _   -> fmap (\_ -> go "Bool") e
        CNil _     -> fmap (\_ -> go "what is this?") e
      where go str = TAdt () (UIdent str) []

{- ******************** -}
-- typecheck utility functions

-- map over all definitions and create a type from every
-- data type declaration that is encountered.
collectTypes :: [Def ()] -> TC [Type ()]
collectTypes ds = return $ catMaybes $ map go ds
  where go (DDataDec _ name vars _) = 
           Just $ TAdt () name (map (TVar ()) vars)
        go _ = Nothing

-- Extract the var from a typed expression
getExpvar :: Exp a -> a
getExpvar e = case e of
    ETup a _      -> a
    ECase a _ _   -> a
    ELet a _ _ _  -> a
    ELetR a _ _ _ -> a
    ELam a _ _    -> a
    EIf a _ _ _   -> a
    ECon a _ _    -> a
    EApp a _ _    -> a
    EOr a _ _     -> a
    EAnd a _ _    -> a
    ERel a _ _ _  -> a
    EAdd a _ _ _  -> a
    EMul a _ _ _  -> a
    ENot a _      -> a
    EVar a _      -> a
    EConst a _    -> a

{- ******************** -}
-- ADT related utility functions

tupExp :: Exp a -> TupExp a
tupExp e = ETupExp (getExpvar e) e

deTupExp :: TupExp a -> Exp a
deTupExp (ETupExp _ e) = e