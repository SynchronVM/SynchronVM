{- | This module implements a system for working with substitutions. A substitution
is a mapping of type variables to types. When two types are unified, the result of that
unification (if successful) should be a substitution that when applied to the two
types makes them both equal. -}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module CamIoT.Typecheck.Substitution where

import CamIoT.Internal.Syntax
import CamIoT.Pretty.Syntax
import CamIoT.Typecheck.Environment

import qualified Data.Map as Map
import qualified Data.Set as Set

{- | A substitution is used during type checking and unification. A substitution is a
map from identifiers (type variables) to types. -}
type Subst = Map.Map Ident Type

-- | The trivial substitution is just an empty map. It has no mappings at all.
unitsub :: Subst
unitsub = Map.empty

{- | Composition of substitutions, which is left-biased. Being left-biased means that
we apply the substitution @s1@ to @s2@ before we perform the union of that with @s1@. -}
compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

{- | Show instance for `Subst`. Since `Subst` is a type synonym for `Map Ident Type`,
which already has a `Show` instance, we must annotate this instance with
@{-# OVERLAPPING#-}@ to give this once precedense. -}
instance {-# OVERLAPPING #-} Show Subst where
  show s = unlines $
           map (\(t1,t2) -> concat [ printTree t1
                                   , " ~> "
                                   , printTree t2]) $
           Map.toList s

{- | A class of types that are substitutable. Being substitutable means at the very
least that you can apply a substitution to it. Some of them will also be able to
tell you which their free type variables are, but not all of them. This should in
all fairness be turned into two type classes. -}
class Substitutable a where
  apply :: Subst -> a -> a   -- ^ Apply a substitution to an object of type @a@
  ftv :: a -> Set.Set Ident  -- ^ Get the free type variables in the object of type @a@

-- | If @a@ is @Substitutable@, so if a list of @a@s.
instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)
  ftv     = foldr (Set.union . ftv) Set.empty

-- | Types are substitutable.
instance Substitutable Type where
  {- | Applying a substitution to a type only does something in the case that the type
  is a type variable and the type variable has a mapping in the substitution. In that
  case, the type variable i replaced with the mapping in the substitution. -}
  apply s t = case t of
    TVar id     -> Map.findWithDefault (TVar id) id s
    TInt        -> TInt
    TFloat      -> TFloat
    TBool       -> TBool
    TNil        -> TNil
    TLam t1 t2  -> TLam (apply s t1) (apply s t2)
    TAdt uid ts -> TAdt uid (apply s ts)
    TTup ts     -> TTup $ map (apply s) ts

  {- | The free type variables of a type is simply the set of type variables, as
  in a type there are no places where a type variable can be bound. -}
  ftv t = case t of
    TVar id     -> Set.singleton id
    TInt        -> Set.empty
    TFloat      -> Set.empty
    TBool       -> Set.empty
    TNil        -> Set.empty
    TLam t1 t2  -> Set.union (ftv t1) (ftv t2)
    TAdt uid ts -> Set.unions $ map ftv ts
    TTup ts     -> Set.unions $ map ftv ts

-- | Patterns are substitutable.
instance Substitutable (Pat Type) where
  apply s p = case p of
    PVar a id     -> PVar (apply s a) id
    PNil a        -> PNil (apply s a)
    PConst a l    -> PConst (apply s a) l
    PWild a       -> PWild (apply s a)
    PAs a id p    -> PAs (apply s a) id (apply s p)
    PAdt a uid ps -> PAdt (apply s a) uid $ map (apply s) ps
    PTup a ps     -> PTup (apply s a) $ map (apply s) ps

  -- We will never actually need this function for patterns
  ftv p = undefined

-- | Expressions are substitutable
instance Substitutable (Exp Type) where
  apply s e = case e of
    EVar a id      -> EVar (apply s a) id
    ECon a uid     -> ECon (apply s a) uid
    ELit a l       -> ELit (apply s a) l
    ECase a e pms  -> ECase a (apply s e) $ map (\(p,e) -> (apply s p, apply s e)) pms
    ETup a es      -> ETup (apply s a) $ map (apply s) es
    EBin a e1 e2 o -> EBin (apply s a) (apply s e1) (apply s e2) (apply s o)
    EUn a e o      -> EUn (apply s a) (apply s e) (apply s o)
    ELam a p e     -> ELam (apply s a) (apply s p) (apply s e)
    EApp a e1 e2   -> EApp (apply s a) (apply s e1) (apply s e2)
    ELet a p e1 e2 -> ELet (apply s a) (apply s p) (apply s e1) (apply s e2)
    EIf a e1 e2 e3 -> EIf (apply s a) (apply s e1) (apply s e2) (apply s e3)

  ftv e = undefined

-- | Binary operators are substitutable.
instance Substitutable (Binop Type) where
  apply s op = case op of
    Add a -> Add $ apply s a
    Sub a -> Sub $ apply s a
    Mul a -> Mul $ apply s a
    Div a -> Div $ apply s a
    OLT a -> OLT $ apply s a
    OLE a -> OLE $ apply s a
    OGT a -> OGT $ apply s a
    OGE a -> OGE $ apply s a
    OEQ a -> OEQ $ apply s a
    And a -> And $ apply s a
    Or  a -> Or  $ apply s a

  ftv op = undefined

-- | Unary operators are substitutable.
instance Substitutable (Unop Type) where
  apply s op = case op of
    Not a -> Not $ apply s a

  ftv op = undefined

{- | Top level definitions are substitutable. Will throw an error if @apply@ is applied
to a datatype declaration. -}
instance Substitutable (Def Type) where
  apply s d = case d of
    DTypeSig id t         -> DTypeSig id t
    DEquation t id args e -> DEquation (apply s t) id (map (apply s) args) (apply s e)

  ftv = undefined

-- | Functions are substitutable.
instance Substitutable (Function Type) where
  apply s f =
    f { equations = map (\(as, b) -> (map (apply s) as, apply s b)) (equations f)
      }

  ftv = undefined

-- | A function definition is substitutable.
instance Substitutable ([Pat Type], Exp Type) where
  apply s (args, body) = (map (apply s) args, apply s body)

  ftv = undefined

-- | An entire program is substitutable.
instance Substitutable (Program Type) where
  apply s p = p { functions = map (apply s) (functions p)
                , main      = apply s (main p)
                }

  ftv = undefined

{- | Schemas can be substituted by first removing any mapping from type variables
in the substitution which are bound by the type schema. E.g if the substitution
contains a mapping from @a@ to @Int@, and the schema is @forall a . a -> Int@, then
the mapping from @a@ to @Int@ is removed before substituting. -}
instance Substitutable Schema where
  apply s (Forall vars t) = Forall vars $ apply s' t
    where s' = foldr Map.delete s vars

  ftv (Forall vars t) = ftv t `Set.difference` Set.fromList vars

-- | Environments are substitutable.
instance Substitutable Env where
  apply s (Env m1 m2) = Env (Map.map (apply s) m1) m2

  -- Not sure we need to account for the FTV in the constructors, they will
  -- be constant
  ftv (Env m1 m2) = ftv $ Map.elems m1
