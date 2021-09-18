{- | This module exposes datatypes and functionality that is associated with
typechecking. -}
module CamIoT.Typecheck.Environment where

import           CamIoT.Internal.Syntax
import           CamIoT.Pretty.Syntax
import           CamIoT.Typecheck.TCError

import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

-- * Schemas

{- | A schema is a type that also contains information about which type variables are
bound and which are free. E.g the type schema of

@
id x = x
@

is

@
forall x . x -> x
@

If we did not use type schemas like this we would not be able to use

@
id : a -> a
@

with more
than one distinct type. We would in the case of

@
id True
@

query the environment for the type of

@
id
@

and get

@
a -> a
@

back. We would then unify the type @x@ with @Bool@ and record that in our substitution.
If we now do

@
id 5
@

we get the type of @id@, but it has now been unified to be of type @Bool -> Bool@, so the
application @id 5@ is no longer valid. Schemas solve this by abstracting over the free
type variables.

If we query the environment for the type of

@
id : forall x . x -> x
@

the bound type variables will be instantiated with fresh type variables, e.g

@
id : a -> a
@

We can unify @a@ with @Bool@ perfectly fine, still, but when we try to typecheck the
application

@
id 5
@

we get a new, fresh type of id,

@
id : b -> b
@

which we can unify with @Int -> Int@.
-}
data Schema = Forall [Ident] Type

getTypeWithoutInstantiate :: Schema -> Type
getTypeWithoutInstantiate (Forall _ t) = t

instance Show Schema where
  show (Forall vars typ) = concat
    ["forall [", intercalate "," (map printTree vars), "] . ", printTree typ]

-- * Type checking monad

-- | The state maintained by the state monad-part of the `TC` monad during type checking
data TCState = TCState
  { namegen :: Int                 -- ^ Counter used to generate fresh names
    {- | Arity of the different algebraic datatypes. E.g
    
    @
    Map.fromList $ [ ("Maybe", 1)
                   , ("Either", 2)
                   , (Int, 0)
                   ]
    @
    -}
  , tycons  :: Map.Map UIdent Int
  }

{- | The type checking monad! We use

  1. @Except@ to report errors
  2. @State@ for generaing fresh type variables
  3. @Reader@ for the typing environment

-}
type TC a = ExceptT TCError (StateT TCState (Reader Env)) a

-- | Need this instance for the do notation, sigh
instance MonadFail Identity where
  fail = error

-- * Type variable generation

-- | Infinite string of unique words @["a0", "a1", ..., "b0", ..., "ab0", ...]@
letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

-- | Generate a fresh type variable
fresh :: TC Type
fresh = do
  s <- get
  put $ s { namegen = namegen s + 1 }
  return $ TVar (Ident (letters !! namegen s))

-- * Typing environment management

{- | Typing environent. The typing environment contains a mapping from identifiers to
schemas and from uppercase identifiers to schemas (constructors). -}
data Env = Env (Map.Map Ident Schema) (Map.Map UIdent Schema)

-- | The empty environment
emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

instance Show Env where
  show (Env identifiers constructors) = concat
    [ "Identifiers:\n"
    , intercalate
      "\n"
      (map (\(id, t) -> printTree id ++ " : " ++ show t)
           (Map.toList identifiers)
      )
    , "\n"
    , "Constructors:\n"
    , intercalate
      "\n"
      (map (\(id, t) -> printTree id ++ " : " ++ show t)
           (Map.toList constructors)
      )
    ]

{- | Does the constructor already exist in the typing environment? This is used for
verifying the correctness of datatype declarations, that the constructors are unique. -}
existsCon :: UIdent -> TC Bool
existsCon uid = do
  (Env _ env) <- ask
  case Map.lookup uid env of
    Just _  -> return True
    Nothing -> return False

-- | Look up the arity of a data constructor
lookupTyconArity :: UIdent -> TC Int
lookupTyconArity uid = do
  st <- get
  case Map.lookup uid (tycons st) of
    Just i  -> return i
    Nothing -> throwError $ UndeclaredTycon uid

-- | Record the arity of a data costructor
extendTyconArity :: UIdent -> Int -> TC ()
extendTyconArity uid arity = do
  st <- get
  case Map.lookup uid (tycons st) of
    Just _  -> throwError $ DuplicateTycon uid
    Nothing -> put $ st { tycons = Map.insert uid arity (tycons st) }

-- | A map that maps binary operators to their type schemas
binoptypes :: Map.Map (Binop ()) Schema
binoptypes =
  Map.fromList
    $ [ (Add (), Forall [a] $ TLam ta (TLam ta ta))
      , (Sub (), Forall [a] $ TLam ta (TLam ta ta))
      , (Mul (), Forall [a] $ TLam ta (TLam ta ta))
      , (Div (), Forall [a] $ TLam ta (TLam ta ta))
      , (OLT (), Forall [a] $ TLam ta (TLam ta TBool))
      , (OLE (), Forall [a] $ TLam ta (TLam ta TBool))
      , (OGT (), Forall [a] $ TLam ta (TLam ta TBool))
      , (OGE (), Forall [a] $ TLam ta (TLam ta TBool))
      , (OEQ (), Forall [a] $ TLam ta (TLam ta TBool))
      , (And (), Forall [] $ TLam TBool (TLam TBool TBool))
      , (Or () , Forall [] $ TLam TBool (TLam TBool TBool))
      ]
 where
  a :: Ident
  a = Ident "a"

  ta :: Type
  ta = TVar a

{- | Candidate types for binary operators. When a binary operator has had an inferred
type, that type needs to be unified with any of the entries in the list of candidate
types of a binary operator. -}
binopCandidates :: Binop () -> [Type]
binopCandidates op = case op of
  Add _ -> [intintint, floatfloatfloat]
  Sub _ -> [intintint, floatfloatfloat]
  Mul _ -> [intintint, floatfloatfloat]
  Div _ -> [floatfloatfloat]
  OLT _ -> [intintbool, floatfloatbool]
  OLE _ -> [intintbool, floatfloatbool]
  OGT _ -> [intintbool, floatfloatbool]
  OGE _ -> [intintbool, floatfloatbool]
  OEQ _ -> [intintbool, floatfloatbool, boolboolbool]
  And _ -> [boolboolbool]
  Or  _ -> [boolboolbool]
 where
  intintint       = TLam TInt (TLam TInt TInt)
  floatfloatfloat = TLam TFloat (TLam TFloat TFloat)
  intintbool      = TLam TInt (TLam TInt TBool)
  floatfloatbool  = TLam TFloat (TLam TFloat TBool)
  boolboolbool    = TLam TBool (TLam TBool TBool)

-- | Look up the type of a unary operator
lookupUnop :: Unop () -> TC Type
lookupUnop op = undefined

-- | Map that maps unary operators to their type schemas
unoptypes :: Map.Map (Unop ()) Schema
unoptypes = Map.fromList $ [(Not (), Forall [] $ TLam TBool TBool)]

{- | Candidate types of unary operators. The same conditions that are described in the
documentation for `binopCandidates` apply here as well. -}
unopCandidates :: Unop () -> [Type]
unopCandidates op = case op of
  Not () -> [TLam TBool TBool]

-- | Extend the typing environment with a new mapping from an identifier to a schema
extend :: Env -> Ident -> Schema -> Env
extend (Env m1 m2) id schema = Env (Map.insert id schema m1) m2

-- | Restrict the typing environment by removing a mapping from an identifer
restrict :: Env -> Ident -> Env
restrict (Env m1 m2) id = Env (Map.delete id m1) m2

-- | Perform a typechecking computation in an extended environment
inEnv :: Ident -> Schema -> TC a -> TC a
inEnv id schema ma = inEnvMany [(id, schema)] ma

{- | Perform a typechecking computation in an environment that has been extended with
zero or more mappings from identifiers to schemas. -}
inEnvMany :: [(Ident, Schema)] -> TC a -> TC a
inEnvMany xs ma =
  let scope e =
        foldl (\e' (id, schema) -> extend (restrict e' id) id schema) e xs
  in  local scope ma

{- | Perform a typechecking computation after having extended the environment with
the data constructors that are in scope. -}
withConstructors :: [ADT] -> TC a -> TC a
withConstructors adts = local
  (\(Env m1 m2) -> Env m1 $ Map.fromList allConsSchemas)
 where
  adtToCons :: ADT -> [(UIdent, Schema)]
  adtToCons (_, vars, cons) = map (\(con, t) -> (con, Forall vars t)) cons

  allConsSchemas :: [(UIdent, Schema)]
  allConsSchemas = concat $ map adtToCons adts
