module Monomorphisation.Environment where

import Parser.AbsTinyCamiot
import Typechecker.AstUtils

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer

data MState = MState {
              -- | Counter used to generate fresh variable names
              counter      :: Int
              -- | Map that maps function names to their definitions
            , functions    :: Map.Map Ident [Def Type]
              -- | Map that maps a function and its type to a new identifier
            , newFunctions :: Map.Map (Ident, Type) Ident
              -- | Map that maps an old constructor and its type to a new constructor
            , newConstructors :: Map.Map (UIdent, Type) UIdent
            }

mutRecFunctionMap :: [Def Type] -> Map.Map Ident [Def Type]
mutRecFunctionMap defs =
  Map.fromList $ concatMap (\(DMutRec tydefs) -> map getpairs tydefs) mutrecdefs
  where
    mutrecdefs = filter isMutRec defs

    isMutRec :: Def Type -> Bool
    isMutRec (DMutRec _) = True
    isMutRec _ = False

    getpairs :: (Def Type, [Def Type]) -> (Ident, [Def Type])
    getpairs (DTypeSig ident _, defs) = (ident, defs)

-- | Create a map from function names to their definitions
functionsMap :: [Def Type] -> Map.Map Ident [Def Type]
functionsMap defs = Map.fromList pairs
  where
      tmp   = filter (not . (isDataDec .||. isMutRec)) $ groupAsFunctions defs
      pairs = map (\fun@(d:_) -> (name d, fun)) tmp

      isDataDec :: [Def Type] -> Bool
      isDataDec (DDataDec _ _ _:_) = True
      isDataDec _                  = False

      isMutRec :: [Def Type] -> Bool
      isMutRec (DMutRec _ :_) = True
      isMutRec _              = False

      name :: Def Type -> Ident
      name (DTypeSig id _)      = id
      name (DEquation _ id _ _) = id
      name (DForeignType id _)  = id

      (.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
      (.||.) f g a = f a || g a

type M a = StateT MState (
             WriterT [Def Type] IO) a

-- | Run a monomorphising computation and return the result, and the new counter
runM :: M a -> MState -> IO (a, Int)
runM ma st = do
    let wa = runStateT ma st
    ((a, c),_) <- runWriterT wa
    return (a, counter c)

-- | Returns True if the identifier has a polymorphic type in the map.
hasPolymorphicType :: Ident -> M Bool
hasPolymorphicType id = do
    types <- gets functions
    case Map.lookup id types of
        Just d  -> case d of
            (DTypeSig _ t:_)      -> return $ containsTypeVariable t
            (DEquation t _ _ _:_) -> return $ containsTypeVariable t
            (DForeignType _ t:_)  -> return False
        Nothing -> return False

-- | Generate a fresh name
fresh :: M Ident
fresh = do
    st <- get
    put $ st { counter = counter st + 1}
    return $ Ident $ "v" ++ show (counter st)
