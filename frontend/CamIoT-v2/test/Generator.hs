{-# LANGUAGE FlexibleInstances #-}
module Generator where

import           CamIoT.Internal.Syntax
import           CamIoT.Pretty.Syntax

import           Test.QuickCheck         hiding ( Function )

import           Data.Foldable

instance {-# OVERLAPPING #-} Show (Def ()) where
    show d = printTree d

instance Show (Function ()) where
    show f = printTree f

instance Show (Program ()) where
    show = printTree

type Variables = [(Ident, Type)]

data St = St
    { variables :: Variables  -- ^ Variables in scope
    , counter   :: Int        -- ^ Name-generating state
    }

freshFunName :: St -> (Ident, St)
freshFunName = freshIdentifier "fun"

freshVarName :: St -> (Ident, St)
freshVarName = freshIdentifier "var"

freshIdentifier :: String -> St -> (Ident, St)
freshIdentifier prefix st =
    let id = Ident $ prefix ++ show (counter st)
    in  (id, st { counter = counter st + 1 })

withVars :: St -> [(Ident, Type)] -> St
withVars st vars = st { variables = variables st ++ vars }

basictype :: Gen Type
basictype = elements [TInt, TFloat, TBool, TNil]

instance Arbitrary (Program ()) where
    arbitrary = genProgram $ St [] 0

genProgram :: St -> Gen (Program ())
genProgram st = do
    (funs, st') <- genFuns st
    (mainfun, _, _) <- genFun st'
    return $ Program [] (map fst funs) mainfun

genFuns :: St -> Gen ([(Function (), Type)], St)
genFuns st = do
    num <- choose (1, 5)
    genFuns' st num
  where
    genFuns' :: St -> Int -> Gen ([(Function (), Type)], St)
    genFuns' st  0 = return ([], st)
    genFuns' st n = do
        (fun, t, st') <- genFun st
        (rest, st'')          <- genFuns' st' (n - 1)
        return ((fun, t) : rest, st'')

genFun :: St -> Gen (Function (), Type, St)
genFun st = do
    let (f, st') = freshFunName st
    numargs      <- choose (0, 3) :: Gen Int
    (args, st'') <- foldlM
        (\(pats, s) i -> do
            (p, t, s') <- genArg s
            return ((p, t) : pats, s')
        )
        ([], st')
        [0 .. numargs]
    restype <- basictype
    let vars = returnBindings args
    body <- genExpOfType restype (withVars st'' vars) 0
    let (arguments, argtypes) = unzip args
    let fun                   = Function f [(arguments, body)] Nothing
    let typeOfFun             = funtype $ argtypes ++ [restype]
    let newst = st { variables = (f, typeOfFun) : variables st }
    return (fun, typeOfFun, st'')
  where
    returnBindings :: [(Pat (), Type)] -> [(Ident, Type)]
    returnBindings []                    = []
    returnBindings ((PVar _ id, t) : ps) = (id, t) : returnBindings ps
    returnBindings (_              : ps) = returnBindings ps

genArg :: St -> Gen (Pat (), Type, St)
genArg st =
    oneof
        $ [ do
              let (id, st') = freshVarName st
              t <- basictype
              return (PVar () id, t, st')
          , do
              blit <- LBool <$> arbitrary
              return (PConst () blit, TBool, st)
          , do
              intlit <- LInt <$> arbitrary
              return (PConst () intlit, TInt, st)
          , do
              floatlit <- LFloat <$> arbitrary
              return (PConst () floatlit, TFloat, st)
          , return (PConst () LNil, TNil, st)
          ]

genExpOfType :: Type -> St -> Int -> Gen (Exp ())
genExpOfType t st 0 = do
    oneof
        $  (if not $ null variablesOfCorrectType
               then
                   [ -- reference a variable
                     do
                         (id, _) <- elements variablesOfCorrectType
                         return $ EVar () id
                   ]
               else []
           )
        ++ -- generate a literal 
           (if isLitType t then [ELit () <$> litOfType t] else [])

  where
    variablesOfCorrectType :: Variables
    variablesOfCorrectType = filter (\(id, t') -> t == t') $ variables st
genExpOfType _ _ _ = undefined

-- | Is the type a simple type that represents a literal?
isLitType :: Type -> Bool
isLitType t = case t of
    TVar id -> True
    TInt    -> True
    TFloat  -> True
    TBool   -> True
    TNil    -> True
    _       -> False

-- | Generate a literal of a specific type
litOfType :: Type -> Gen Lit
litOfType t = case t of
    TVar id -> arbLit
    TInt    -> LInt <$> arbitrary
    TFloat  -> LFloat <$> arbitrary
    TBool   -> LBool <$> arbitrary
    TNil    -> return LNil
    _       -> theError
  where
    theError = error $ concat
        [ "Generator.litOfType error ---\n"
        , " litOfType applied to type that is not a literal type\n"
        , " can only apply litOfType to Int, Float, Bool, () or type variable, in which"
        , " case any of the previous types will be generated"
        ]

-- | Generate arbitrary literal
arbLit :: Gen Lit
arbLit = oneof
    [LInt <$> arbitrary, LFloat <$> arbitrary, LBool <$> arbitrary, return LNil]
