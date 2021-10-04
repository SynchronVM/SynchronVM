module Monomorphisation.Monomorphise where

import Parser.AbsTinyCamiot
import Parser.PrintTinyCamiot
import Typechecker.AstUtils
import Monomorphisation.Environment
import LambdaLifting.LambdaLifting
import Typechecker.Substitution ( Substitutable(apply) )
import HindleyMilner.HM ( runUnify )
import HindleyMilner.TypeInference () -- importing an instance

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.List

import System.IO.Unsafe
import qualified Debug.Trace as DT

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

{- | Entrypoint for the monoomrphisation pass. Takes the current state (for name generation)
and the program and returns the monomorphised program and the new state. -}
monomorphise :: Int -> [Def Type] -> IO ([Def Type], Int)
monomorphise counter defs = runM (monomorphiseFunctions defs) state
  where
    state =
      MState
        counter
        (functionsMap defs `Map.union` mutRecFunctionMap defs) -- we do not allow polymorphic mutrec funs now
                                                               -- but we will add this functionality in the future
        Map.empty
        Map.empty

{- | Monomorphise all the functions in the program. The input is given as a flattened
list of definitions, which are then grouped together and monomorphised on a group basis. -}
monomorphiseFunctions :: [Def Type] -> M [Def Type]
monomorphiseFunctions defs = do defs'   <- withoutPoly grouped
                                (_, w)  <- listen $ mapM_ monomorphiseFunction defs'
                                (m, w') <- listen   generateDataDeclarations
                                let res = map (updateTypeOfDef m) $ w' ++ w
                                return res
  where
    -- | Group the definitions together by name, if any.
    grouped :: [[Def Type]]
    grouped = groupAsFunctions defs

    -- | Returns all monomorphic functions.
    withoutPoly :: [[Def Type]] -> M [[Def Type]]
    withoutPoly funs = filterM pred funs
      where
        -- mutrec functions will be grouped such that it will be a list
        -- containing only one element the DMutRec with its body
        pred :: [Def Type] -> M Bool
        pred [(DMutRec _)] = return True -- making mutrec functions monomorphic for the time being
        pred fun = if not (isDataDec fun)
                     then do b <- hasPolymorphicType (name fun)
                             return $ not b
                     else return False

    -- | Returns True if the group in question is a data type declaration.
    isDataDec :: [Def Type] -> Bool
    isDataDec (DDataDec _ _ _:_) = True
    isDataDec _                  = False

    -- | Returns the name of a group, if any.
    name :: [Def Type] -> Ident
    name (DTypeSig id _:_)      = id
    name (DEquation _ id _ _:_) = id

{- | Monomorphise a single function. If it uses any polymorphic functions they will be
specialized and a new function call replaces the old one. The specialized functions
themselves will be recursively monomorphised. -}
monomorphiseFunction :: [Def Type] -> M ()
monomorphiseFunction defs = do defs' <- monoAllDefinitions defs --undefined
                               tell defs'
  where
    {- | Monomorphise all definitions. We use this helper method because we want to `tell`
    the entire group in one go, three lines up. This function will not `tell` anything, but
    will only process the group and return the result. -}
    monoAllDefinitions :: [Def Type] -> M [Def Type]
    monoAllDefinitions []     = return []
    monoAllDefinitions (d:ds) = case d of
      DDataDec _ _ _           -> monoAllDefinitions ds >>= \ds' -> return (d:ds')
      DTypeSig id t            -> monoAllDefinitions ds >>= \ds' -> return (d:ds')
      DEquation t id args body -> do body' <- removePolyAppsInExp body
                                     args' <- mapM removePolyADTsInPats args
                                     let d' = DEquation t id args' body'
                                     ds'   <- monoAllDefinitions ds
                                     return (d':ds')
      DMutRec defs -> do
        defs' <- monomorphiseMutRecs defs
        ds' <- monoAllDefinitions ds
        return $ (DMutRec defs') : ds'
        where
          monomorphiseMutRecs :: [(Def Type, [Def Type])] -> M [(Def Type, [Def Type])]
          monomorphiseMutRecs [] = return []
          monomorphiseMutRecs ((tysig,defs):xs) = do
            (tysig':defs') <- monoAllDefinitions (tysig:defs)
            xs' <- monomorphiseMutRecs xs
            return $ (tysig',defs') : xs'

    {- | This function will traverse an expression and return a new one, where the new one
    have had all applications of polymorphic functions replaced with applications of
    specialized versions instead. -}
    removePolyAppsInExp :: Exp Type -> M (Exp Type)
    removePolyAppsInExp e = case e of
          ECase a e pms   -> do e'   <- removePolyAppsInExp e
                                pms' <- mapM removePolyAppsInPms pms
                                return $ ECase a e' pms'
          ELet a p e1 e2  -> do e1' <- removePolyAppsInExp e1
                                e2' <- removePolyAppsInExp e2
                                p'  <- removePolyADTsInPats p
                                return $ ELet a p' e1' e2'
          ELetR a p e1 e2 -> do e1' <- removePolyAppsInExp e1
                                e2' <- removePolyAppsInExp e2
                                p'  <- removePolyADTsInPats p
                                return $ ELetR a p' e1' e2'
          ELam a id e     -> do e' <- removePolyAppsInExp e
                                return $ ELam a id e'
          EIf a e1 e2 e3  -> do e1' <- removePolyAppsInExp e1
                                e2' <- removePolyAppsInExp e2
                                e3' <- removePolyAppsInExp e3
                                return $ EIf a e1' e2' e3'
          EApp a e1 e2    -> do e1' <- removePolyAppsInExp e1
                                e2' <- removePolyAppsInExp e2
                                return $ EApp a e1' e2'
          EOr a e1 e2     -> do e1' <- removePolyAppsInExp e1
                                e2' <- removePolyAppsInExp e2
                                return $ EOr a e1' e2'
          EAnd a e1 e2    -> do e1' <- removePolyAppsInExp e1
                                e2' <- removePolyAppsInExp e2
                                return $ EAnd a e1' e2'
          ERel a e1 o e2  -> do e1' <- removePolyAppsInExp e1
                                e2' <- removePolyAppsInExp e2
                                return $ ERel a e1' o e2'
          EAdd a e1 o e2  -> do e1' <- removePolyAppsInExp e1
                                e2' <- removePolyAppsInExp e2
                                return $ EAdd a e1' o e2'
          EMul a e1 o e2  -> do e1' <- removePolyAppsInExp e1
                                e2' <- removePolyAppsInExp e2
                                return $ EMul a e1' o e2'
          ETup a texps    -> do texps' <- mapM removePolyAppsInExp texps
                                return $ ETup a texps'
          ENot a e        -> do e' <- removePolyAppsInExp e
                                return $ ENot a e'
          EVar a id       -> do id' <- specializeFunction id a
                                return $ EVar a id'
          EUVar a uid     -> do uid' <- renameUID uid a
                                return $ EUVar a uid'
          EConst _ _      -> return e

    -- | This function does the same as above, but on values wrapped in a PM
    removePolyAppsInPms :: PatMatch Type -> M (PatMatch Type)
    removePolyAppsInPms (PM p e) = do e' <- removePolyAppsInExp e
                                      p' <- removePolyADTsInPats p
                                      return (PM p' e')

    removePolyADTsInPats :: Pat Type -> M (Pat Type)
    removePolyADTsInPats p = case p of
        PConst _ _             -> return p
        PVar _ _               -> return p
        PZAdt a uid            -> do uid'   <- renameUID uid a
                                     return  $ PZAdt a uid'
        PNAdt a uid apats      -> {- When we encounter a uid in an expression it will have a
                                     function type. When we encounter them in a pattern they
                                     will be fully applied, meaning not of a function type.
                                     I create the complete function type of the unapplied
                                     constructor on these first two lines such that I can
                                     compare the type to those I encounter in expression. It
                                     was arbitrarily chosen - I could have done it the other
                                     way around also. -}
                                  do let argtypes = map getPatVar apats
                                     let fulltype = functionType argtypes a

                                     uid'   <- renameUID uid fulltype
                                     apats' <- mapM removePolyADTsInPats apats
                                     return  $ PNAdt a uid' apats'
        PWild _                -> return p
        PNil _                 -> return p
        PTup a tuppats         -> do tuppats' <- mapM removePolyADTsInPats tuppats
                                     return $ PTup a tuppats'
        PLay a ident pat       -> do pat' <- removePolyADTsInPats pat
                                     return $ PLay a ident pat'

{- | The first argument is a function name and the second argument is the type which we
use that function with. If the function is polymorphic and no specialized function exists for it,
specialize it and recursively monomorphise it, and then return the name name. If the function has
already been specialized, return the name of the specialized function.-}
specializeFunction :: Ident -> Type -> M Ident
specializeFunction oldid newt = do
    b <- hasPolymorphicType oldid
    -- Is it a function at all? Is it a polymorphic one? We don't want to specialize variables.
    if b
      then do
        -- See if this function has been specialized before, and if a new ID exists
        nfunctions <- gets newFunctions
        case Map.lookup (oldid, newt) nfunctions of
          Just newid -> return newid
          
          {- If that is not the case, we need to specialize the function and update
          our state with the name of the specialized function. -}
          Nothing    -> do
            funs <- gets functions
            case Map.lookup oldid funs of
              Just defs -> do newid      <- fresh
                              defs'      <- renameArguments defs
                              defs''     <- rewriteType defs' newt
                              let defs''' = renameFunction defs'' newid
                              let newfuns = Map.insert (oldid, newt) newid nfunctions
                              modify $ \st -> st { newFunctions = newfuns}
                              monomorphiseFunction defs'''
                              return newid
              Nothing   -> error $ "function " ++ printTree oldid ++ " does not exist..."
      else return oldid

  where
      {- | Rename the arguments of an equation and every occurence of them in the
      equations body. -}
      renameArguments :: [Def Type] -> M [Def Type]
      renameArguments []     = return []
      renameArguments (d:ds) = case d of
        DTypeSig _ _             -> do ds' <- renameArguments ds
                                       return (d:ds')
        DEquation t id args body -> do (args', env) <- renamePatterns args
                                       let body' = renameExp env body
                                       ds' <- renameArguments ds
                                       return $ DEquation t id args' body' : ds'
          where
            -- | Rename a list of patterns.
            renamePatterns :: [Pat Type] -> M ([Pat Type], Map.Map Ident Ident)
            renamePatterns ps = do
              (ps', maps) <- unzip <$> mapM renamePattern ps
              return (ps', Map.unions maps)

            {- | If the pattern is a variable, a new name is generated to be used instead,
            and a map associating the old variable with the new one is produced. -}
            renamePattern :: Pat Type -> M (Pat Type, Map.Map Ident Ident)
            renamePattern p = case p of
              PConst _ _             -> return (p, Map.empty)
              PVar a id              -> do id' <- fresh
                                           return (PVar a id', Map.singleton id id')
              PZAdt a uid            -> return (p, Map.empty)
              PNAdt a uid apats      -> do (apats',m) <- renamePatterns apats
                                           return (PNAdt a uid apats', m)
              PWild _                -> return (p, Map.empty)
              PNil _                 -> return (p, Map.empty)
              PTup a tuppats         -> do (tuppats', m) <- renamePatterns tuppats
                                           return (PTup a tuppats', m)
              PLay a id pat          -> do id' <- fresh
                                           (pat', m) <- renamePattern pat
                                           let m' = Map.singleton id id'
                                           return (PLay a id' pat', Map.union m m')

      {-- | Rewrite the type information in a function. It should go from being
      tagged with polymorphic type information to being tagged with concrete
      type information. -}
      rewriteType :: [Def Type] -> Type -> M [Def Type]
      rewriteType [] _        = return []
      rewriteType (d:ds) newt = case d of
          DTypeSig id t            -> do let d' = DTypeSig id (apply typeEnv t)
                                         ds'   <- rewriteType ds newt
                                         return (d':ds')
          DEquation t id args body -> do let t'    = apply typeEnv t
                                         let args' = map (apply typeEnv) args
                                         let body' = apply typeEnv body
                                         let d'    = DEquation t' id args' body'
                                         ds'      <- rewriteType ds newt
                                         return (d':ds')
        where
          {- | We use the unification-machinery from the typechecker to
          unify the polymorphic type with the specific one, and then get a map
          we can apply over the polymorphic definition. -}
          typeEnv :: Map.Map Ident Type
          typeEnv = case runUnify [[(typeOfDef, newt)]] of
            Just s  -> s
            Nothing -> error $ "Monomorphiser failed unification for " <> show [[(typeOfDef, newt)]]

          -- | Type of the definition
          typeOfDef :: Type
          typeOfDef = case d of
            (DTypeSig _ t)      -> t
            (DEquation t _ _ _) -> t

      -- | Simply rename a function.
      renameFunction :: [Def Type] -> Ident -> [Def Type]
      renameFunction [] _         = []
      renameFunction (d:ds) newid = case d of
        DTypeSig _ t            -> DTypeSig newid t            : renameFunction ds newid
        DEquation t _ args body -> DEquation t newid args body : renameFunction ds newid

{- | Takes a uid and a type and returns a new uid (if none already exists, in which case that
one is returned). -}
renameUID :: UIdent -> Type -> M UIdent
renameUID uid@(UIdent name) t = do
  cons <- gets newConstructors
  case Map.lookup (uid, t) cons of
    Just uid' -> return uid'
    Nothing   -> do c <- gets counter
                    let uid' = UIdent $ name ++ show c
                    let cons' = Map.insert (uid, t) uid' cons
                    modify $ \st -> st { newConstructors = cons', counter = c+1}
                    return uid'

generateDataDeclarations :: M (Map.Map Type Type)
generateDataDeclarations = do
    adts        <- newADTs
    (defs, env) <- genDataDecs adts
    tell defs
    return env
  where
    {- | Returns a list of pairs where the first element is a list of new constructors and
    the second element is the original type the original constructor had. -}
    newADTs :: M [([(UIdent, Type)], Type)]
    newADTs = do
        newcs      <- gets newConstructors
        let grouped = groupBy pred $ Map.toList newcs
        return $ map collapse grouped
      where
        {- | Compares entries after which ADT they produced, e.g
        Just : Int -> Maybe Int and Nothing : Maybe Bool, here we would
        compare Maybe Int and Maybe Bool. -}
        pred :: ((UIdent, Type), UIdent) -> ((UIdent, Type), UIdent) -> Bool
        pred ((_,t1),_) ((_,t2),_) = unwrapFunction t1 == unwrapFunction t2

        {- | Forgets the names of the old constructors, and retains only one copy
        of the result ADT.-}
        collapse :: [((UIdent, Type), UIdent)] -> ([(UIdent, Type)], Type)
        collapse xs = let (pairs, newc) = unzip xs
                          (_, types@(t:_))    = unzip pairs
                          newc'         = zip newc types
                      in (newc', unwrapFunction t)

    {- | Given a tuple where the first element is a list of pairs of constructors and
    their types, and the second component is the ADT the old constructor created, such as
    Maybe Int, Either Int Bool etc, generate a new specific name such as Maybe13 for the
    specialized constructors that create Maybe13's. -}
    genDataDec :: ([(UIdent, Type)], Type) -> M (Def Type, Map.Map Type Type)
    genDataDec (cons, TAdt (UIdent name) _) = do
        adt <- newADT
        let newType = TAdt adt []
        return (DDataDec adt [] constructors, Map.singleton oldType newType)
      where
        constructors :: [ConstructorDec]
        constructors = map (uncurry ConstDec) cons

        newADT :: M UIdent
        newADT = do
          c <- gets counter
          modify $ \st -> st { counter = counter st + 1}
          return $ UIdent $ name ++ show c
        
        oldType :: Type
        oldType = unwrapFunction $ snd $ head cons
      
    -- | The same as the above, but for many.
    genDataDecs :: [([(UIdent, Type)], Type)] -> M ([Def Type], Map.Map Type Type)
    genDataDecs []         = return ([], Map.empty)
    genDataDecs (dec:decs) = do
        (def, m)   <- genDataDec dec
        (defs, ms) <- genDataDecs decs
        return (def:defs, Map.union m ms)

{- | Takes a map of old ADT types to new ADT types, e.g Maybe Int to Maybe13,
and update all occurences of e.g Maybe Int in the definition to Maybe13. -}
updateTypeOfDef :: Map.Map Type Type -> Def Type -> Def Type
updateTypeOfDef m d = case d of
    DDataDec uid args cons   -> DDataDec uid args (map (updateCons m) cons)
    DTypeSig id t            -> DTypeSig id (updateType m t)
    DEquation t id args body -> let t'    = updateType m t
                                    args' = map  (fmap (updateType m)) args
                                    body' = fmap (updateType m) body
                                in DEquation t' id args' body'
    DMutRec tydefs -> DMutRec (map updateTypeMut tydefs)
  where
      -- | Updates the type in a constructor declaration
      updateCons :: Map.Map Type Type -> ConstructorDec -> ConstructorDec
      updateCons m (ConstDec uid t) = ConstDec uid (updateType m t)

      updateType :: Map.Map Type Type -> Type -> Type
      updateType m = mapType $ rewriteType m

      rewriteType :: Map.Map Type Type -> Type -> Type
      rewriteType m t = case Map.lookup t m of
          Just t' -> t'
          Nothing -> t

      mapType :: (Type -> Type) -> Type -> Type
      mapType f t = case t of
          TLam t1 t2    -> let t1' = mapType f t1
                               t2' = mapType f t2
                               t'  = TLam t1' t2'
                           in f t'
          TVar _        -> f t
          TNil          -> f t
          TAdt uid typs -> let typs' = map (mapType f) typs
                               t'    = TAdt uid typs'
                           in f t'
          TTup typs     -> let typs' = map (mapType f) typs
                               t'    = TTup typs'
                           in f t'
          TBool         -> f t
          TInt          -> f t
          TFloat        -> f t

      updateTypeMut :: (Def Type, [Def Type]) -> (Def Type, [Def Type])
      updateTypeMut (ty, defs) =
        (updateTypeOfDef m ty, map (updateTypeOfDef m) defs)
