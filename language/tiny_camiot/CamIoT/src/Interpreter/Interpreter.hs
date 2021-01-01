module Interpreter.Interpreter where

import Parser.AbsTinyCamiot
import Parser.PrintTinyCamiot
import Desugaring.AST

import qualified Data.Map as Map
import Control.Monad.Reader

--data Environment = Environment (Map.Map Ident Value) (Map.Map Ident Value)
data Environment = Environment (Map.Map Ident Value) (Map.Map Ident Value)

data Value = VInt Integer
           | VFloat Double
           | VBool Bool
           | VTup Value Value
           | VAdt UIdent (Maybe Value)
           | VNil
           | VClosure (SExp SType) (Map.Map Ident Value)
  deriving (Eq)

instance Show Value where
    show (VInt i)            = show i
    show (VFloat d)          = show d
    show (VBool b)           = show b
    show (VTup v1 v2)        = "(" ++ show v1 ++ "," ++ show v2 ++ ")"
    show (VAdt uid Nothing)  = show uid
    show (VAdt uid (Just v)) = "(" ++ show uid ++ " " ++ show v ++ ")"
    show VNil                = "()"
    show _                   = "can not render function"

-- | This is just to save on LOC in the interpreter
instance Num Value where
    (VInt i1)   + (VInt i2)   = VInt   $ i1 + i2
    (VFloat d1) + (VFloat d2) = VFloat $ d1 + d2
    _ + _                     = error "bad adition"
    (VInt i1)   - (VInt i2)   = VInt   $ i1 - i2
    (VFloat d1) - (VFloat d2) = VFloat $ d1 - d2
    _ - _                     = error "bad subtraction"
    (VInt i1)   * (VInt i2)   = VInt   $ i1 * i2
    (VFloat d1) * (VFloat d2) = VFloat $ d1 * d2
    _ * _                     = error "bad multiplication"
    negate (VInt i)           = VInt (-i)
    negate (VFloat d)         = VFloat (-d)
    negate _                  = error "bad negation"
    abs (VInt i)              = VInt   $ abs i
    abs (VFloat d)            = VFloat $ abs d
    abs _                     = error "bad abs"
    signum (VInt i)           = VInt   $ signum i
    signum (VFloat d)         = VFloat $ signum d
    signum _                  = error "bad signum"
    fromInteger i             = VInt i

instance Ord Value where
    (VInt i1)   <= (VInt i2)   = i1 <= i2
    (VFloat d1) <= (VFloat d2) = d1 <= d2
    _           <= _           = error "bad ord operation"

type Interp a = Reader Environment a

interpret :: SExp SType -> IO ()
interpret e = do
    let v = runReader (interpExp e) (Environment Map.empty Map.empty)
    putStrLn $ show v

extendFun :: Ident -> Value -> Interp a -> Interp a
extendFun id v = local (\(Environment funs vars) -> Environment (Map.insert id v funs) vars)

extendFuns :: Map.Map Ident Value -> Interp a -> Interp a
extendFuns funs = local (\(Environment funs' vars) -> Environment (Map.union funs funs') vars)

extendVar :: Ident -> Value -> Interp a -> Interp a
extendVar id v = local (\(Environment funs vars) -> Environment funs (Map.insert id v vars))

extendVars :: Map.Map Ident Value -> Interp a -> Interp a
extendVars vars = local (\(Environment funs vars') -> Environment funs (Map.union vars vars'))

withVars :: Map.Map Ident Value -> Interp a -> Interp a
withVars vars = local (\(Environment funs _) -> Environment funs vars)

lookupVar :: Ident -> Interp Value
lookupVar id = do
    (Environment funs vars) <- ask
    case Map.lookup id vars of
        Just v  -> return v
        Nothing -> case Map.lookup id funs of
            Just v  -> return v
            Nothing -> error $ "cannot find variable " ++ printTree id

bindPatAndValue :: SPat SType -> Value -> Map.Map Ident Value
bindPatAndValue (SPVar _ id) v               = Map.singleton id v
bindPatAndValue (SPTup _ p1 p2) (VTup v1 v2) = let m1 = bindPatAndValue p1 v1
                                                   m2 = bindPatAndValue p2 v2
                                               in Map.union m1 m2
bindPatAndValue (SPLay _ id p) v             = let m1 = bindPatAndValue p v
                                                   m2 = Map.singleton id v
                                               in Map.union m1 m2
bindPatAndValue (SPNAdt _ uid1 (Just p))
                (VAdt     uid2 (Just v))     = bindPatAndValue p v
bindPatAndValue _ _                          = Map.empty

interpCase :: SExp SType -> [SPatMatch SType] -> Interp Value
interpCase e pms = do v <- interpExp e
                      go v pms
  where
      go :: Value -> [SPatMatch SType] -> Interp Value
      go v []     = error "non-exhaustive patterns in case"
      go v ((SPM p e):bs) = if eqBranch v p
          then extendVars (bindPatAndValue p v) $ interpExp e
          else go v bs

      -- | Checks if the value matches the shape of the pattern
      eqBranch :: Value -> SPat SType -> Bool
      eqBranch _                    (SPWild _)               = True
      eqBranch _                    (SPVar _ _)              = True
      eqBranch VNil                 (SPNil _)                = True
      eqBranch (VBool True)         (SPConst _ CTrue)        = True
      eqBranch (VBool False)        (SPConst _ CFalse)       = False
      eqBranch (VInt i1)            (SPConst _ (CInt i2))    = i1 == i2
      eqBranch (VFloat d1)          (SPConst _ (CFloat d2))  = d1 == d2
      eqBranch (VTup v1 v2)         (SPTup _ p1 p2)          = eqBranch v1 p1 && eqBranch v2 p2
      eqBranch (VAdt uid1 Nothing)  (SPNAdt _ uid2 Nothing)  = uid1 == uid2
      eqBranch (VAdt uid1 (Just v)) (SPNAdt _ uid2 (Just p)) = uid1 == uid2 && eqBranch v p
      eqBranch v                    (SPLay _ _ p)            = eqBranch v p
      eqBranch _                    _                        = False

interpExp :: SExp SType -> Interp Value
interpExp e = case e of
    SECase  a e pms    -> interpCase e pms
    SELet   a p e1 e2  -> do v1 <- interpExp e1
                             let m = bindPatAndValue p v1
                             extendFuns m $ interpExp e2
    SELetR  a p e1 e2  -> do v1 <- interpExp e1
                             let m = bindPatAndValue p v1
                             extendFuns m $ interpExp e2
    SELam   a p e      -> do (Environment _ vars) <- ask
                             return $ VClosure (SELam a p e) vars
    SEIf    a e1 e2 e3 -> do v1 <- interpExp e1
                             v2 <- interpExp e2
                             v3 <- interpExp e3
                             case v1 of
                                 VBool True  -> interpExp e2
                                 VBool False -> interpExp e3
    SEApp   a e1 e2    -> do v1 <- interpExp e1
                             case v1 of
                                 VClosure (SELam a (SPVar _ id) b) vars -> do
                                     v2 <- interpExp e2
                                     withVars (Map.insert id v2 vars) (interpExp b)
                                     --extendVar id v2 $ interpExp b 
                                 _ -> error "can only apply functions - should not end up here"
    SEOr    a e1 e2    -> do v1 <- interpExp e1
                             v2 <- interpExp e2
                             case (v1,v2) of
                                 (VBool b1, VBool b2) -> return $ VBool $ b1 || b2
    SEAnd   a e1 e2    -> do v1 <- interpExp e1
                             v2 <- interpExp e2
                             case (v1,v2) of
                                 (VBool b1, VBool b2) -> return $ VBool $ b1 && b2
    SERel   a e1 op e2 -> do v1 <- interpExp e1
                             v2 <- interpExp e2
                             case op of
                                 LTC _ -> return $ VBool $ v1 <  v2
                                 LEC _ -> return $ VBool $ v1 <= v2
                                 GTC _ -> return $ VBool $ v1 >  v2
                                 GEC _ -> return $ VBool $ v1 >= v2
                                 EQC _ -> return $ VBool $ v1 == v2
    SEAdd   a e1 op e2 -> do v1 <- interpExp e1
                             v2 <- interpExp e2
                             case op of
                                 Plus _  -> return $ v1 + v2
                                 Minus _ -> return $ v1 - v2
    SEMul   a e1 op e2 -> do v1 <- interpExp e1
                             v2 <- interpExp e2
                             case op of
                                 Times _  -> return $ v1 * v2
                                 Div _    -> case (v1, v2) of
                                     (VInt i1,   VInt i2)   -> return $ VInt   $ i1 `div` i2
                                     (VFloat d1, VFloat d2) -> return $ VFloat $ d1 / d2
    SETup   a e1 e2    -> do v1 <- interpExp e1
                             v2 <- interpExp e2
                             return $ VTup v1 v2
    SENot   a e        -> do v <- interpExp e
                             case v of
                                 VBool b -> return $ VBool (not b)
    SETag a uid e      -> case e of
                            Just e' -> interpExp e' >>= \v -> return $ VAdt uid (Just v)
                            Nothing -> return $ VAdt uid Nothing
    SEVar   a id       -> do v <- lookupVar id
                             case v of
                                 VClosure e' env -> withVars env (interpExp e')
                                 _               -> return v
    SEConst a c        -> case c of
        CInt i   -> return $ VInt i
        CFloat d -> return $ VFloat d
        CTrue    -> return $ VBool True
        CFalse   -> return $ VBool False
        CNil     -> return VNil