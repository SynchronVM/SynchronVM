module Monomorphisation.Monomorphise (monomorphise) where

import Monomorphisation.Environment

import Parser.AbsTinyCamiot
import Parser.PrintTinyCamiot ( printTree )
import Typechecker.Substitution ( Substitutable(apply) )
import HindleyMilner.HM ( runUnify )
import HindleyMilner.TypeInference () -- importing an instance
import LambdaLifting.LambdaLifting ( renameExp )
import Typechecker.AstUtils
    ( getPatVar, functionType, unwrapFunction, containsTypeVariable, groupAsFunctions )

import qualified Data.Map as Map
import Data.List ( groupBy, nubBy, sortBy )
import Data.Maybe ( fromMaybe )

import Control.Monad.State ( gets, MonadState(put, get), liftIO, zipWithM )

--------------- Monomorphise functions ---------------

-- | Monomorphize a program.
monomorphise :: [Def Type] -> Int -> IO [Def Type]
monomorphise defs st = runM (monomorphise' defs) state 
  where
      types   = Map.fromList $ gatherTypes defs
      state = MState st types Map.empty Map.empty

-- | Create the computation that monomorphizes a program.
monomorphise' :: [Def Type] -> M [Def Type]
monomorphise' defs = do
    -- monomorphize function applications
    specialized <- monoFunctions defs
    {- remove all datatype declarations from the program, as we will generate
    new ones in the last step. -}
    let noDataDecs =  filter (\d -> case d of DDataDec _ _ _ -> False; _ -> True) specialized
    -- monomorphize the ADTs in the program
    monoADTs noDataDecs

-- | Monomorphize function applications and remove polymorphic functions.
monoFunctions :: [Def Type] -> M [Def Type]
monoFunctions defs = do
    -- remove applications of polymorphic functions
    defs'          <- removePolyApps defs
    -- fetch which polymorphic functions has to be specialized to what
    toSpecialize   <- gets functionsToCreate
    -- group definitions together as functions
    let functions  =  groupAsFunctions defs'
    -- actually specialize the polymorphic functions
    concat . concat <$> mapM (`monomorphizeFunction` toSpecialize) functions

{- | This function takes a list of definitions that together make up _one_ function,
 and the map created from `removePolyApps`. If the input function is a polymorphic one
 and `removePolyApps` has registered applications of it, it will be specialized as many
 times as it was applied to different types. If it is not a polymorphic one it is returned
 as is. -}
monomorphizeFunction :: [Def Type] -> Map.Map Ident [(Ident, Type)] -> M [[Def Type]]
monomorphizeFunction fun apps = if isPolyMorphicDef (head fun)
                                  then funs   -- Either we return the original definition
                                  else return [fun]  -- Or we return the new specialized ones
  where
      -- | Name of this function
      name :: Ident
      name = case head fun of
          DTypeSig id _      -> id
          DEquation _ id _ _ -> id

      -- | The specialized versions that we want to create
      typs :: [(Ident, Type)]
      typs = fromMaybe [] $ Map.lookup name apps

      -- | The specialized versions
      funs :: M [[Def Type]]
      funs = zipWithM (\f (id,t) -> mapM (\d -> specialize d t id) f) (repeat fun) typs

{- | This function takes a list of definitions (the original input) and does two things.
It will traverse the AST and look for any function removePolyApps. When an application is
found it will check if the applied function has a polymorphic type. If this is the case,
a new name will be generated that will replace the old one in the AST. After this an
entry will be inserted in a map where we have as key the original function name, and as
value a pair, where the first component is the new name and the second one is the type it
is applied to.

This map will then be used to generate a new function for each entry in the created map.
-}
removePolyApps :: [Def Type] -> M [Def Type]
removePolyApps []     = return []
removePolyApps (d:ds) = case d of
    DDataDec uid ids cd     -> removePolyApps ds >>= \ds' -> return (d:ds')
    DTypeSig id t           -> removePolyApps ds >>= \ds' -> return (d:ds')
    DEquation typ id args e -> do e' <- if isPolyMorphicDef d
                                                 then return e
                                                 else removePolyAppsInExp e
                                  ds' <- removePolyApps ds
                                  return $ DEquation typ id args e' : ds'
  where
      removePolyAppsInExp :: Exp Type -> M (Exp Type)
      removePolyAppsInExp e = case e of
          ECase a e pms   -> do e'   <- removePolyAppsInExp e
                                pms' <- mapM removePolyAppsInPms pms
                                return $ ECase a e' pms'
          ELet a p e1 e2  -> do e1' <- removePolyAppsInExp e1
                                e2' <- removePolyAppsInExp e2
                                return $ ELet a p e1' e2'
          ELetR a p e1 e2 -> do e1' <- removePolyAppsInExp e1
                                e2' <- removePolyAppsInExp e2
                                return $ ELetR a p e1' e2'
          ELam a id e     -> do e' <- removePolyAppsInExp e
                                return $ ELam a id e'
          EIf a e1 e2 e3  -> do e1' <- removePolyAppsInExp e1
                                e2' <- removePolyAppsInExp e2
                                e3' <- removePolyAppsInExp e3
                                return $ EIf a e1' e2' e3'
          EApp a (EVar b id) e -> do e'     <- removePolyAppsInExp e
                                     isPoly <- hasPolymorphicType id
                                     if isPoly
                                         then do id' <- newId id b
                                                 return $ EApp a (EVar b id') e'
                                             --do id' <- fresh
                                             --    emitType id (id', b)
                                             --    return $ EApp a (EVar b id') e'
                                         else return $ EApp a (EVar b id) e'
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
          EVar _ _        -> return e
          EUVar _ _       -> return e
          EConst _ _      -> return e

      -- | This function does the same as above, but on values wrapped in a PM
      removePolyAppsInPms :: PatMatch Type -> M (PatMatch Type)
      removePolyAppsInPms (PM p e) = removePolyAppsInExp e >>= \e' -> return (PM p e')

{- | Take a polymorphic definition and the type of which we want it to be,
and replace the type information with the new correct one. -}
specialize :: Def Type      -- ^ Definition, expected to be of polymorphic type
        -> Type          -- ^ The type we want it to be, the shape is expected to be the same
        -> Ident         -- ^ The name we should give the new typechanged definition
        -> M (Def Type)  -- ^ The new renamed definition. The function body is also renamed.
specialize d t newid = case d of
        DTypeSig _ ft          -> return $ DTypeSig newid (apply (mapping ft t) ft)
        DEquation ft id args e -> let newt  = apply (mapping ft t) ft
                                      newe  = renameExp (Map.singleton id newid) e
                                      newe' = apply (mapping ft t) newe
                                      args' = map (apply (mapping ft t)) args
                                  in renameEquation $ DEquation newt newid args' newe'
        DDataDec _ _ _         -> return d
  where
      mapping :: Type -> Type -> Map.Map Ident Type
      mapping t1 t2 = case runUnify [[(t1, t2)]] of
          Just subst -> subst
          Nothing  -> error $ "Types could not be unified " ++ 
                               printTree t1 ++ " and " ++
                               printTree t2

      renamePat :: Pat Type -> M (Map.Map Ident Ident, Pat Type)
      renamePat p = case p of
          PConst _ _      -> return (Map.empty, p)
          PVar a id       -> fresh >>= \id' -> return (Map.singleton id id', PVar a id')
          PZAdt _ _       -> return (Map.empty, p)
          PNAdt a uid apats -> do (maps, apats') <- unzip <$> mapM renamePat apats
                                  return (Map.unions maps, PNAdt a uid apats')
          PWild a         -> return (Map.empty, p)
          PNil a          -> return (Map.empty, p)
          PTup a tpats    -> do (maps, tpats') <- unzip <$> mapM renamePat tpats
                                return (Map.unions maps, PTup a tpats')
          PLay a id p'    -> do id' <- fresh
                                let m = Map.singleton id id'
                                (m', p'') <- renamePat p'
                                return (Map.union m m', PLay a id' p')
        
      renameEquation :: Def Type -> M (Def Type)
      renameEquation (DEquation t id args body) = do
          (m, args') <- unzip <$> mapM renamePat args
          let m'      = Map.unions m
          let body'   = renameExp m' body
          return (DEquation t id args' body')


--------------- Monomorphize algebraic data types ---------------

monoADTs :: [Def Type] -> M [Def Type]
monoADTs defs = do
    defs' <- removePolyConstructorApps defs
    constrinfo <- gets (Map.toList . newConstructors)
    (defs'', env) <- generateADTs constrinfo
    let res = transformAllTypes (`monoType` env) $ defs'' ++ defs'
    --let res = fmap (fmap (`monoType` env)) $ defs'' ++ defs' 
    return res

{- | Remove uses of polymorphic constructors and replace with version that should be
specialized. -}
removePolyConstructorApps :: [Def Type] -> M [Def Type]
removePolyConstructorApps []     = return []
removePolyConstructorApps (d:ds) = case d of
    DDataDec _ _ _        -> removePolyConstructorApps ds >>= \ds' -> return $ (d:ds')
    DTypeSig _ _          -> removePolyConstructorApps ds >>= \ds' -> return $ (d:ds')
    DEquation t id args e -> do
        args' <- mapM monoPat args
        e'    <- monoExp e
        ds'   <- removePolyConstructorApps ds
        return $ DEquation t id args' e' : ds'
  where
      -- | Monomorphizes ADTs in expressions.
      monoExp :: Exp Type -> M (Exp Type)
      monoExp e = case e of
          ECase a e pms   -> do e'   <- monoExp e
                                pms' <- mapM monoPM pms
                                return $ ECase a e' pms'
          ELet a p e1 e2  -> do p'  <- monoPat p
                                e1' <- monoExp e1
                                e2' <- monoExp e2
                                return $ ELet a p' e1' e2'
          ELetR a p e1 e2 -> undefined
          ELam a id e     -> monoExp e >>= \e' -> return $ ELam a id e'
          EIf a e1 e2 e3  -> do e1' <- monoExp e1
                                e2' <- monoExp e2
                                e3' <- monoExp e3
                                return $ EIf a e1' e2' e3'
          EApp a e1 e2    -> do e1' <- monoExp e1
                                e2' <- monoExp e2
                                return $ EApp a e1' e2'
          EOr a e1 e2     -> do e1' <- monoExp e1
                                e2' <- monoExp e2
                                return $ EOr a e1' e2'
          EAnd a e1 e2    -> do e1' <- monoExp e1
                                e2' <- monoExp e2
                                return $ EAnd a e1' e2'
          ERel a e1 o e2  -> do e1' <- monoExp e1
                                e2' <- monoExp e2
                                return $ ERel a e1' o e2'
          EAdd a e1 o e2  -> do e1' <- monoExp e1
                                e2' <- monoExp e2
                                return $ EAdd a e1' o e2'
          EMul a e1 o e2  -> do e1' <- monoExp e1
                                e2' <- monoExp e2
                                return $ EMul a e1' o e2'
          ETup a texps    -> mapM monoExp texps >>= \texps' -> return $ ETup a texps'
          ENot a e        -> monoExp e >>= \e' -> return $ ENot a e'
          EVar _ _        -> return e
          EUVar a uid     -> do uid' <- getNewConstructor (uid, a)
                                return $ EUVar a uid'
          EConst _ _      -> return e

      -- | Monomorphizes ADTs in patterns.
      monoPat :: Pat Type -> M (Pat Type)
      monoPat p = case p of
        PConst _ _             -> return p
        PVar _ _               -> return p
        PZAdt a uid            -> do uid' <- getNewConstructor (uid, a)
                                     return $ PZAdt a uid'
        PNAdt a uid apats      -> do apats' <- mapM monoPat apats

                                     {- The variable `a` here has the type of the fully
                                     applied constructor, while we want the type of just
                                     the constructor. To recreate the type of the unapplied
                                     constructor we create a function type from all the
                                     argument types to a. -}
                                     let argtyps = map getPatVar apats
                                     let ctype   = functionType argtyps a
                                     uid'   <- getNewConstructor (uid, ctype)
                                     return $ PNAdt a uid' apats'
        PWild _                -> return p
        PNil _                 -> return p
        PTup a tuppats         -> do
            tuppats' <- mapM monoPat tuppats
            return $ PTup a tuppats'
        PLay a ident pat       -> do
            pat' <- monoPat pat
            return $ PLay a ident pat'

      -- | Monomorphizes ADTs in case pattern-match clauses.
      monoPM :: PatMatch Type -> M (PatMatch Type)
      monoPM (PM p e) = do
          p' <- monoPat p
          e' <- monoExp e
          return $ PM p' e'

-- | If t exists in m, return the value associated with it. Otherwise just return m.
monoType :: Type -> Map.Map Type Type -> Type
monoType t m = fromMaybe t (Map.lookup t m)

{- | ((Old constructor name, type it is applied to), name given to the new constructor
that should be generated). -}
type NewConstructor = ((UIdent,Type), UIdent)

{- | Take a list of ((old constructor, type), new constructor) pairs and generate the
data type declarations that are appropriate and a map mapping old types to new types. -}
generateADTs :: [NewConstructor] -> M ([Def Type], Map.Map Type Type)
generateADTs inp = do groups'          <- mapM convertGroup groups
                      let defs         =  map createDataDec groups'
                      let environment  =  env groups'
                      return (defs, environment)
  where
      {- Group the new constructors together by what type they produce
         E.g [((Just, Int -> Maybe Int),_),
              ((Nothing, Maybe Int),_),
              ((Just, Bool -> Maybe Bool),_)]
         would produce [[((Just, Int -> Maybe Int),_),
                         ((Nothing, Maybe Int),_)]
                         ,
                         ((Just, Bool -> Maybe Bool),_)
         and we would know we need to generate two instances of the Maybe datatype.
      -}
      groups :: [[NewConstructor]]
      groups = let sorted = sortBy (\((_,one),_) ((_,two),_) -> compare (unwrapFunction one) (unwrapFunction two)) inp
               in  groupBy (\((_,one),_) ((_,two),_) -> unwrapFunction one == unwrapFunction two) sorted

      -- Take a group and generate a new name for the type of this group
      convertGroup :: [NewConstructor] 
                   -> M ([NewConstructor]  -- Pairs of ((old constructor, type), new constructor)
                        , UIdent           -- New name of type
                        , Type)            -- Type that was created by the constructor
      convertGroup grp = do
          st <- get
          put $ st { counter = counter st + 1}
          let (uid, t) = originalUId
          return (grp, UIdent (uid ++ show (counter st)), t)
        where
            -- | What type does this group create?
            originalUId :: (String, Type)
            originalUId = let t@(TAdt (UIdent uid) _) = unwrapFunction $ (snd . fst) (head grp)
                          in (uid, t)

      -- Take a group of constructors and create the new type definition for them
      createDataDec :: ([NewConstructor], UIdent, Type) -> Def Type
      createDataDec (constrs, newtypename, _) = DDataDec newtypename [] constructors
        where
            constructors = map (\((_,t),uid) -> ConstDec uid t ) constrs

      {- | Create a map that maps old type names to new type names. E.g List Int could
      now have been given the name List5 or something similar. -}
      env :: [([NewConstructor], UIdent, Type)] -> Map.Map Type Type
      env groups = Map.fromList $ map typeOfGroup groups
        where
            typeOfGroup :: ([NewConstructor], UIdent, Type) -> (Type, Type)
            typeOfGroup (_, newtypename, oldtype) = (oldtype, TAdt newtypename [])


{- | Accept as input a function that maps types to types. We assume that the function will
only map types of form `TAdt _ _` to a new type, so we only match on that case in the
`retypeType` subfunction. -}
transformAllTypes :: (Type -> Type) -> [Def Type] -> [Def Type]
transformAllTypes _ []     = []
transformAllTypes f (d:ds) = case d of
    DDataDec uid vars cd  -> DDataDec uid vars (map retypeConsDec cd) : ds'

    DTypeSig id t         -> DTypeSig id (retypeType t) : ds'

    DEquation t id args e -> let t'    = retypeType t
                                 e'    = fmap retypeType e
                                 args' = map (fmap retypeType) args
                             in DEquation t' id args' e' : ds'
  where
      retypeConsDec :: ConstructorDec -> ConstructorDec
      retypeConsDec (ConstDec uid t) = ConstDec uid (retypeType t)

      retypeType :: Type -> Type
      retypeType t = case t of
          TLam t1 t2 -> TLam (retypeType t1) (retypeType t2)
          TAdt _ _   -> f t
          TTup typs  -> TTup $ map retypeType typs
          _          -> t

      ds' :: [Def Type]
      ds' = transformAllTypes f ds