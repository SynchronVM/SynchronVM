{- | This module implements alpha renaming of programs expressed by the
"CamIoT.Internal.Syntax" module. -}
module CamIoT.Rename.Syntax where

import           CamIoT.Internal.Syntax
import           CamIoT.Rename.Environment
import           CamIoT.Util

import qualified Data.Map                      as Map

import           Control.Monad.Except
import           Control.Monad.Reader

-- * Renaming helper functions

{- | Takes an identifier and returns the new name of that identifier, if any exist.
If there's no new name to be found, an error has occured and an error is subsequently
raised. -}
rename :: Ident -> Rename Ident
rename id = do
  env <- ask
  case Map.lookup id env of
    Just id' -> return id'
    Nothing  -> throwError $ NotRenamed id

{- | Perform a renaming computation where the renaming environment has been extended with
the mappings given as the first parameter to this function. -}
withMappings :: [(Ident, Ident)] -> Rename a -> Rename a
withMappings ids = local (inserted . deleted)
 where
  {- | Take the renaming state and delete any occurence of the new mappings from this
     state. -}
  deleted :: RenameState -> RenameState
  deleted env = foldl (\e' (to, _) -> Map.delete to e') env ids

  -- | Take the renaming state and insert all the new mappings into it
  inserted :: RenameState -> RenameState
  inserted env = foldl (\e' (to, from) -> Map.insert to from e') env ids

-- * Rename programs

-- | Alpha rename a program in the language
renameProgram :: Program a -> Rename (Program a)
renameProgram p = do
  functions' <- renameFunctions (functions p ++ [main p])
  return $ p { functions = init functions', main = last functions' }

-- * Rename functions

-- | Should the identifier @id@ be renamed?
shouldRename :: Ident -> Rename Bool
shouldRename id = do
  e <- ask
  case Map.lookup id e of
    Just _  -> return False
    Nothing -> return True

-- | Alpha rename a function in the language
renameFunction :: Function a -> Rename (Function a)
renameFunction f = do
  b <- shouldRename $ name f
  if b
    then do
      id  <- freshIdentifier
      eqs <- withMappings [(name f, id)] $ mapM renameDef $ equations f
      return $ f { equations = eqs, name = id }
    else do
      eqs <- mapM renameDef $ equations f
      return $ f { equations = eqs }
 where
     {- | Alpha rename a definition in the language, giving it a new name and
     renaming its arguments before rewriting the equation body. -}
  renameDef :: ([Pat a], Exp a) -> Rename ([Pat a], Exp a)
  renameDef (args, body) = do
    (args', mappings) <- unzip <$> mapM renamePat args
    body'             <- withMappings (concat mappings) $ renameExp body
    return (args', body')

renameFunctions :: [Function a] -> Rename [Function a]
renameFunctions []       = return []
renameFunctions (f : fs) = do
  f'  <- renameFunction f
  fs' <- withMappings [(name f, name f')] $ renameFunctions fs
  return $ f' : fs'

-- * Rename patterns

{- | Take a pattern and return the same pattern but alpha renamed. Also return all the
new mappings generated while renaming the pattern, so that the environment can be
extended with this information before proceeding. The strategy taken here is that at
all sites where identifiers are produced, replace that identifier with a fresh
identifier and return the pair @(oldid, newid)@.

The patterns that construct identifiers are `PVar` and `PAs`. -}
renamePat :: Pat a -> Rename (Pat a, [(Ident, Ident)])
renamePat p = case p of
  PConst a ct -> return (p, [])
  PVar   a id -> do
    id' <- freshIdentifier
    return (PVar a id', [(id, id')])
  PNil  a    -> return (p, [])
  PWild a    -> return (p, [])
  PAs a id p -> do
    id'         <- freshIdentifier
    (p', names) <- renamePat p
    return (PAs a id' p', (id, id') : names)
  PAdt a uid aps -> do
    (aps', names) <- unzip <$> mapM renamePat aps
    return (PAdt a uid aps', concat names)
  PTup a ps -> do
    (ps', names) <- unzip <$> mapM renamePat ps
    return (PTup a ps', concat names)

-- * Rename expressions

-- | Alpha rename an expression
renameExp :: Exp a -> Rename (Exp a)
renameExp e = case e of
  ELet a p e1 e2 -> do
    (p', mappings) <- renamePat p
    e1'            <- renameExp e1
    e2'            <- withMappings mappings $ renameExp e2
    return $ ELet a p' e1' e2'
  ELam a p e -> do
    (p', mappings) <- renamePat p
    e'             <- withMappings mappings $ renameExp e
    return $ ELam a p' e'
  EApp a e1 e2 -> do
    e1' <- renameExp e1
    e2' <- renameExp e2
    return $ EApp a e1' e2'
  EBin a e1 e2 o -> do
    e1' <- renameExp e1
    e2' <- renameExp e2
    return $ EBin a e1' e2' o
  EUn a e o -> do
    e' <- renameExp e
    return $ EUn a e' o
  ETup a es -> do
    es' <- mapM renameExp es
    return $ ETup a es'
  EVar a id -> do
    id' <- rename id
    return $ EVar a id'
  ECon a uid    -> return $ ECon a uid
  ELit a c      -> return $ ELit a c
  ECase a e pms -> do
    e'   <- renameExp e
    pms' <- mapM renamePatternMatch pms
    return $ ECase a e' pms'
  EIf a e1 e2 e3 -> do
    e1' <- renameExp e1
    e2' <- renameExp e2
    e3' <- renameExp e3
    return $ EIf a e1' e2' e3'

{- | Alpha rename a pattern match clause. Rename the bound variables in the pattern and
extend the environment with the new information before renaming the expression. -}
renamePatternMatch :: (Pat a, Exp a) -> Rename (Pat a, Exp a)
renamePatternMatch (p, e) = do
  (p', mappings) <- renamePat p
  e'             <- withMappings mappings $ renameExp e
  return (p', e')
