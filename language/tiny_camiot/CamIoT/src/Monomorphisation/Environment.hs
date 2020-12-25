module Monomorphisation.Environment where

import Parser.AbsTinyCamiot
import Parser.PrintTinyCamiot
import Typechecker.AstUtils

import Control.Monad.State
import qualified Data.Map as Map

import System.IO.Unsafe
import Data.List ( find, nubBy )

trace :: Show a => a -> a
trace x = unsafePerformIO $ putStrLn (show x) >> return x

trace2 :: Print a => a -> a
trace2 x = unsafePerformIO $ putStrLn (printTree x) >> return x

data MState = MState { -- Counter to generate fresh variable names
                       counter  :: Int

                       -- ^ For monomorphizing functions

                       -- Types of all definitions in the program
                     , types    :: Map.Map Ident Type
                       {- | A map of keys and values. The keys are original function
                       names of polymorphic functions, while the values are pairs of
                       new names and types, which will be used to generate new
                       specialized functions, with that name and of that type. -}
                     , functionsToCreate :: Map.Map Ident [(Ident, Type)]

                       -- ^ For monomorphizing datatypes
                       -- Map from old constructornames to new constructornames
                     , newConstructors :: Map.Map (UIdent, Type) UIdent
                     }

type M a = StateT MState IO a

-- | Run a monomorphisation computation.
runM :: M a -> MState -> IO a
runM = evalStateT

-- | Generate a fresh variable name.
fresh :: M Ident
fresh = do
    st <- get
    put $ st { counter = counter st + 1}
    return $ Ident $ "v" ++ show (counter st)

{- | Given the name of a function and the type it is applied to, return the
name of the new lifted function. If the (name, type) combo has not been given a
new name before, a new name is generated and inserted into the current context. -}
newId :: Ident -> Type -> M Ident
newId id typ = do
  st <- get
  let toc = functionsToCreate st
  case Map.lookup id toc of
    Just new -> case find ((==) typ . snd) new of
      Just (id',_) -> return id'
      Nothing      -> do let id' = Ident $ "v" ++ show (counter st)
                         let m = Map.singleton id [(id', typ)]
                         put $ st { functionsToCreate = Map.unionWith (++) toc m
                                  , counter           = counter st + 1
                                  }
                         return id'
    Nothing  -> do let id' = Ident $ "v" ++ show (counter st)
                   let m = Map.singleton id [(id', typ)]
                   put $ st { functionsToCreate = Map.unionWith (++) toc m
                            , counter           = counter st + 1
                            }
                   return id'

-- | Returns True if the identifier has a polymorphic type in the map.
hasPolymorphicType :: Ident -> M Bool
hasPolymorphicType id = do
    types <- gets types
    case Map.lookup id types of
        Just t  -> return $ containsTypeVariable t
        Nothing -> return False

-- | Returns True if the given equation is a polymorphic definition, otherwise False.
isPolyMorphicDef :: Def Type -> Bool
isPolyMorphicDef d = case d of
    DDataDec _ _ _    -> False
    DTypeSig _ t      -> containsTypeVariable t
    DEquation t _ _ _ -> containsTypeVariable t

getNewConstructor :: (UIdent, Type) -> M UIdent
getNewConstructor pair@(UIdent name, _) = do
    constructors <- gets newConstructors
    case Map.lookup pair constructors of
        Just uid -> return uid
        Nothing  -> do st <- get
                       let uid = UIdent $ name ++ show (counter st)
                       put $ st { counter = counter st + 1
                                , newConstructors = Map.insert pair uid constructors
                                }
                       return uid

-- | This function will traverse the AST and build a map of functions and their types.
gatherTypes :: [Def Type] -> [(Ident, Type)]
gatherTypes ds = nubBy (\(id1,_) (id2,_) -> id1 == id2) (gatherTypes' ds)
  where
    gatherTypes' []     = []
    gatherTypes' (d:ds) = case d of
        DDataDec uid ids cd   -> gatherTypes' ds
        DTypeSig id t         -> (id, t) : gatherTypes' ds
        DEquation t id args e -> (id, t) : gatherTypes' ds