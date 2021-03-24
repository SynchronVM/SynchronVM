module Rewriting.Rewriting where

import Parser.AbsTinyCamiot
import Desugaring.AST
import Control.Monad.State

data RWState = RWState {
               counter :: Int
             }

type RW a = State RWState a

-- | Run a rewrite computation.
runDS :: Int -> RW a -> IO (a, Int)
runDS state ma = do
  let (a, st) = runState ma $ RWState state
  return (a, counter st)

fresh :: RW Ident
fresh = do
    st <- get
    put $ st { counter = counter st + 1}
    return $ Ident $ "v" ++ show (counter st)

rewriteExp :: SExp a -> (SExp a -> RW (SExp a)) -> RW (SExp a)
rewriteExp e f = case e of
    SECase a e' pms  -> do pms' <- mapM (\(SPM p e') -> f e' >>= \e'' -> return (SPM p e'')) pms
                           e''  <- f e'
                           f $ SECase a e'' pms'
    SELet a p e1 e2  -> do e1' <- f e1
                           e2' <- f e2
                           f $ SELet a p e1' e2'
    SELetR a p e1 e2 -> do e1' <- f e1
                           e2' <- f e2
                           f $ SELetR a p e1' e2'
    SELam a p e'     -> do e'' <- f e'
                           f $ SELam a p e''
    SEIf a e1 e2 e3  -> do e1' <- f e1
                           e2' <- f e2
                           e3' <- f e3
                           f $ SEIf a e1' e2' e3'
    SEApp a e1 e2    -> do e1' <- f e1
                           e2' <- f e2
                           f $ SEApp a e1' e2'
    SEOr a e1 e2     -> do e1' <- f e1
                           e2' <- f e2
                           f $ SEOr a e1' e2'
    SEAnd a e1 e2    -> do e1' <- f e1
                           e2' <- f e2
                           f $ SEAnd a e1' e2'
    SERel a e1 op e2 -> do e1' <- f e1
                           e2' <- f e2
                           f $ SERel a e1' op e2'
    SEAdd a e1 op e2 -> do e1' <- f e1
                           e2' <- f e2
                           f $ SEAdd a e1' op e2'
    SEMul a e1 op e2 -> do e1' <- f e1
                           e2' <- f e2
                           f $ SEMul a e1' op e2'
    SETup a e1 e2    -> do e1' <- f e1
                           e2' <- f e2
                           f $ SETup a e1 e2
    SENot a e'       -> f e' >>= \e'' -> f $ SENot a e''
    SEVar a id       -> f e
    SEUVar a uid     -> f e
    SEConst a c      -> f e

{- ********* rewriting examples ********** -}

{- An if expression can be turned into a case expression by inspecting
   the boolean test and creating two branches. -}
removeIf :: SExp SType -> RW (SExp SType)
removeIf e = case e of
    SEIf a e1 e2 e3 -> let b1 = SPM (SPConst STBool CTrue)  e2
                           b2 = SPM (SPConst STBool CFalse) e3
                       in return $ SECase a e1 [b1,b2]
    _               -> return e -- do nothing if it is not an if