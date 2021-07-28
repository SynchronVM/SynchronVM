-- MIT License

-- Copyright (c) 2020 Robert Krook, Abhiroop Sarkar

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
{-# LANGUAGE FlexibleInstances #-}
module Typechecker.Environment
       (
         -- * Type schemes
         Scheme(..)
       , instantiate
       , generalize

         -- * Type checking environment
       , TEnv(..)
       , emptyEnv
       , extend
       , restrict
       , inEnv
       , inEnvMany
       , lookupVar

         -- * Type checking dynamic environment
       , TCState(..)
       , emptyState
       , lookupCons

       , TC()
       , runTC
       , TCError(..)
       , Constraint(..)
       , uni
       , uniMany
       , uniEither
       , letters
       , fresh
       ) where

import Parser.AbsTinyCamiot ( Type(..), UIdent(..), Ident(..) )
import Parser.PrintTinyCamiot ()
import Typechecker.Substitution ( Substitutable(..))
--import Typechecker.Constraint ( Constraint )
import Typechecker.TCUtils ( TCError(..) )

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.State  ( replicateM, MonadState(put, get), StateT, runStateT )
import Control.Monad.Reader ( MonadReader(ask, local), ReaderT, runReaderT )
import Control.Monad.Writer ( WriterT, runWriterT, tell )
import Control.Monad.Except ( MonadError(throwError), ExceptT, runExceptT )

-- | Type schemas, e.g forall a . Maybe a.
data Scheme = Forall [Ident] Type

instance Substitutable Scheme where
    apply s (Forall vars t) = Forall vars $ apply s' t
                              where s' = foldr Map.delete s vars

    ftv (Forall vars t) = ftv t `Set.difference` Set.fromList vars

-- | Type checking environment. Maps identifiers to type schemas.
newtype TEnv = TEnv (Map.Map Ident Scheme)

instance Substitutable TEnv where
  apply s (TEnv env) =  TEnv $ Map.map (apply s) env
  ftv (TEnv env) = ftv $ Map.elems env

-- | Empty environment
-- emptyEnv :: TEnv
-- emptyEnv = TEnv Map.empty

emptyEnv :: TEnv
emptyEnv = TEnv $ Map.fromList $
  [ (Ident "channel", Forall [a] $ TLam unit channel )
  , (Ident "send"   , Forall [a] $ TLam channel (TLam ta unitevent))
  , (Ident "recv"   , Forall [a] $ TLam channel event)
  , (Ident "sync"   , Forall [a] $ TLam event ta)
  , (Ident "choose" , Forall [a] $ TLam event (TLam event event))
  , (Ident "spawn"  , Forall []  $ TLam (TLam unit unit) TInt)
  , (Ident "spawnExternal", Forall [a] $ TLam channel (TLam TInt unit))
  , (Ident "wrap" , Forall [a, b] $ TLam event (TLam (TLam ta tb) eventb))
  , (Ident "syncT", Forall [a]    $ TLam TInt (TLam TInt (TLam event ta)))
  ]
  --  syncT : Time -> Time -> Event a -> a
  --  wrap  : Event a -> (a -> b) -> Event b
  where
    a       = Ident "a"
    b       = Ident "b"
    ta      = TVar a
    tb      = TVar b
    unit    = TNil
    channel = TAdt (UIdent "Channel") [ta]
    event   = TAdt (UIdent "Event")   [ta]
    eventb  = TAdt (UIdent "Event")   [tb]
    unitevent = TAdt (UIdent "Event") [unit]



-- | Extend the environment with a new identifier and type schema
extend :: TEnv -> (Ident, Scheme) -> TEnv
extend (TEnv env) (x, sc) = TEnv $ Map.insert x sc env

-- | Delete an identifier and type schema from the environment.
restrict :: TEnv -> Ident -> TEnv
restrict (TEnv env) var = TEnv $ Map.delete var env

{-- | Run the type checking computation after extending the local environment with a new
identifier and type schema.
-}
inEnv :: (Ident, Scheme) -> TC a -> TC a
inEnv xsc = inEnvMany [xsc]

{-- | Run the type checking computation after extending the local environment with many
new identifier-type schema pairs.
-}
inEnvMany :: [(Ident, Scheme)] -> TC a -> TC a
inEnvMany xs m = do
    let scope e = foldl (\e' (x,sc) -> restrict e' x `extend` (x, sc)) e xs
    local scope m

-- | Instantiate a type schema with fresh type variables, yielding a unique type.
instantiate ::  Scheme -> TC Type
instantiate (Forall vars t) = do
  vars' <- mapM (const fresh) vars
  let s = Map.fromList $ zip vars vars'
  return $ apply s t

-- | Generalise a polymorphic type to a type schema.
generalize :: TEnv -> Type -> Scheme
generalize env t  = Forall vars t
    where vars = Set.toList $ ftv t `Set.difference` ftv env

-- | Fetch the type of the identifier. If it is a polymorphic type, instantiate it.
lookupVar :: Ident -> TC Type
lookupVar x@(Ident name) = do
    (TEnv env) <- ask
    case Map.lookup x env of
        Just s  -> instantiate s
        Nothing -> throwError $ UnboundVariable name

-- | Fetch the type of the constructor. If it is a polymorphic type, instantiate it.
lookupCons :: UIdent -> TC Type
lookupCons con = do
    env <- get
    case Map.lookup con (constructors env) of
        Just t  -> instantiate t
        Nothing -> throwError $ UnboundConstructor con

-- | Dynamic state to use while typechecking. TODO move constructors into the reader state
data TCState = TCState
    { num            :: Int                    -- ^ Internal state used to generate names
    , constructors   :: Map.Map UIdent Scheme  -- ^ Map of constructors and type schemas
    }

-- | Empty state
emptyState :: TCState
emptyState = TCState 0 Map.empty

-- | The type of constraints emitted by the typechecker
data Constraint 
    = C (Type, Type, Maybe (Type -> Type -> Maybe TCError))
    | C2 [Constraint]

-- | Substitutable instance for constraints
instance Substitutable Constraint where
    apply s (C (t1, t2, test)) = C (apply s t1, apply s t2, test)
    apply s (C2 cs)            = C2 (map (apply s) cs)

    ftv (C (t1, t2, _)) = Set.union (ftv t1) (ftv t2)
    ftv (C2 cs)         = Set.unions (map ftv cs)
    
-- | Type checking monad
type TC a = ReaderT TEnv (
            WriterT [Constraint] (
            StateT TCState (
            ExceptT TCError 
            IO))) 
            a

-- | Run a TC computation, discarding the state.
runTC :: TC a -> TEnv -> IO (Either TCError (a, [Constraint]))
runTC tc initEnv = do
  ex <- (runExceptT . flip runStateT emptyState . runWriterT) (runReaderT tc initEnv)
  case ex of
    Left  e -> return $ Left e
    Right r -> return $ Right $ fst r

-- | Emit two types that should be unified
uni :: Type -> Type -> Maybe (Type -> Type -> Maybe TCError) -> TC ()
uni t1 t2 test = tell [C (t1, t2, test)]

{-- | Unify many types. I've (Robert) convinced myself that if you can unify t1 and t2,
and you can unify t2 and t3, then you can surely unify t1 and t3.
-}
uniMany :: [Type] -> Maybe (Type -> Type -> Maybe TCError) -> TC ()
uniMany []       _ = return ()
uniMany [x]      t = uni x x t
uniMany [x,y]    t = uni x y t
uniMany (x:y:xs) t = uni x y t >> uniMany (y:xs) t

{-- | Presented with a list of pairs of types, `uniEither` will try to unify one pair
at a time until one of them succeeds. Used when e.g typechecking `(+)`, which has
the type forall a . a -> a -> a, but must be unified with either Int -> Int -> Int,
or Float -> Float -> Float.
-}
uniEither :: [(Type, Type)] -> TC ()
uniEither cs = tell [C2 (map (\(t1,t2) -> C (t1,t2,Nothing)) cs)]

-- | Helper function used to generate fresh type variables.
letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Generate a fresh type variable.
fresh :: TC Type
fresh = do
  s <- get
  put $ s { num = num s + 1}
  return $ TVar (Ident (letters !! num s))
