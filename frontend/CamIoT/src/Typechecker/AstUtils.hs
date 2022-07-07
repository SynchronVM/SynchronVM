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
module Typechecker.AstUtils
       (
         -- * Transform definitions
         groupAsFunctions

         -- * Fetch functor values
       , getExpVar
       , getPatVar
       , getAddopVar
       , getMulopVar
       , getRelopVar
 
         -- * Fetching type information
       , getPMType
 
         -- * Functions that operate on AST types
       , functionType
       , unwrapFunction
       , countArguments
       , typearguments
       , usesVar
       , recursive

         -- * Aliases for AST types
       , (*->)
       , bool
       , int
       , float
       , containsTypeVariable
       ) where

import Parser.AbsTinyCamiot
    ( PatMatch(..),
      Pat(..),
      RelOp(..),
      MulOp(Div, Times),
      AddOp(Minus, Plus),
      Exp(..),
      Type(TFloat, TVar, TAdt, TNil, TTup, TLam, TBool, TInt),
      Def(DEquation, DTypeSig, DMutRec, DForeignType),
      Ident )
import Typechecker.Substitution ( Substitutable(..) )

import Data.List ( groupBy )

{- | This function will traverse the AST and group definitions together such that
each group represents one function. -}
groupAsFunctions :: [Def Type] -> [[Def Type]]
groupAsFunctions = groupBy f
  where f (DEquation _ n1 _ _) (DEquation _ n2 _ _) = n1 == n2
        f (DTypeSig n1 _) (DEquation _ n2 _ _)      = n1 == n2
        f _ _                                       = False

-- | Returns the functor value of expressions
getExpVar :: Exp a -> a
getExpVar e = case e of
    ETup a _      -> a
    ECase a _ _   -> a
    ELet a _ _ _  -> a
    ELetR a _ _ _ -> a
    ELam a _ _    -> a
    EIf a _ _ _   -> a
    EApp a _ _    -> a
    EOr a _ _     -> a
    EAnd a _ _    -> a
    ERel a _ _ _  -> a
    EAdd a _ _ _  -> a
    EMul a _ _ _  -> a
    ENot a _      -> a
    EUVar a _     -> a
    EVar a _      -> a
    EConst a _    -> a

-- | Returns the functor value of patterns
getPatVar :: Pat a -> a
getPatVar p = case p of
    PConst a _   -> a
    PVar a _     -> a
    PZAdt a _    -> a
    PNAdt a _ _  -> a
    PWild a      -> a
    PNil a       -> a
    PTup a _     -> a
    PLay a _ _   -> a

-- | Returns the functor value of additive operators
getAddopVar :: AddOp a -> a
getAddopVar op = case op of
    Plus a  -> a
    Minus a -> a

-- | Returns the functor value of multiplicative operators
getMulopVar :: MulOp a -> a
getMulopVar op = case op of
  Times a -> a
  Div a   -> a

-- | Returns the functor value of relation operators
getRelopVar :: RelOp a -> a
getRelopVar op = case op of
  LTC a -> a
  LEC a -> a
  GTC a -> a
  GEC a -> a
  EQC a -> a

-- | Returns the type of the expression in a case` pattern match clause
getPMType :: PatMatch Type -> Type
getPMType (PM _ e) = getExpVar e

-- | Builds a function type from the argument types.
functionType 
    :: [Type]  -- ^ Argument types 
    -> Type    -- ^ Result types
    -> Type
functionType xs res = foldr TLam res xs

{-- | If the argument type is a function type, unwrap one argument and perform a
recursive call. Essentially returns the result of a type.
-}
unwrapFunction :: Type -> Type
unwrapFunction (TLam _ t) = unwrapFunction t
unwrapFunction t          = t

{-- | If the argument type is a function type, count the number of arguments
specified in the type.
-}
countArguments :: Type -> Int
countArguments (TLam _ t) = 1 + countArguments t
countArguments _          = 0

typearguments :: Type -> [Type]
typearguments (TLam t1 t2) = t1 : typearguments t2
typearguments _            = []

-- | Operator alias to construct a function type
infixr 8 *->
(*->) :: Type -> Type -> Type
t1 *-> t2 = TLam t1 t2

-- | Alias for type Bool
bool :: Type
bool = TBool --TAdt () (UIdent "Bool") []

-- | Alias for type Int
int :: Type
int = TInt --TAdt () (UIdent "Int") []

-- | Alias for type Float
float :: Type
float = TFloat -- TAdt () (UIdent "Float") []

containsTypeVariable :: Type -> Bool
containsTypeVariable t = case t of
    TLam t1 t2  -> containsTypeVariable t1 || containsTypeVariable t2
    TAdt _ typs -> any containsTypeVariable typs
    TTup typs   -> any containsTypeVariable typs
    TVar _      -> True
    TNil        -> False
    TBool       -> False
    TInt        -> False
    TFloat      -> False

{- | Take a list of definitions that together make up one functions, and return True
if the function is a recursive one. -}
recursive :: [Def a] -> Bool
recursive defs = any (usesVar name) (exps defs)
  where
    -- | Name of the function
    name :: Ident
    name = case head defs of
      DTypeSig id _      -> id
      DEquation _ id _ _ -> id

    -- | The bodies of this function
    exps :: [Def a] -> [Exp a]
    exps []                     = []
    exps (DTypeSig _ _:ds)      = exps ds
    exps (DEquation _ _ _ e:ds) = e : exps ds
    exps (DForeignType _ _:ds)  = exps ds

{-- | Checks if the argument identifier occurs in the argument expression.
Used to check for e.g if a function is recursive.
-}
usesVar
    :: Ident  -- ^ Argument identifier, e.g fac
    -> Exp a  -- ^ Argument expression, e.g n * fac (n-1)
    -> Bool
usesVar id e = case e of
    (ETup a texps)    -> any (usesVar id) texps
    (ECase a e1 br)   -> usesVar id e1 || any (usesVarPatMatch id) br
    (ELet a p e1 e2)  -> usesVarPat id p || usesVar id e1 || usesVar id e2
    (ELetR a p e1 e2) -> usesVarPat id p || usesVar id e1 || usesVar id e2
    (ELam a p e1)     -> usesVarPat id p || usesVar id e1
    (EIf a e1 e2 e3)  -> usesVar id e1 || usesVar id e2 || usesVar id e3
    (EApp a e1 e2)    -> usesVar id e1 || usesVar id e2
    (EOr a e1 e2)     -> usesVar id e1 || usesVar id e2
    (EAnd a e1 e2)    -> usesVar id e1 || usesVar id e2
    (ERel a e1 op e2) -> usesVar id e1 || usesVar id e2
    (EAdd a e1 op e2) -> usesVar id e1 || usesVar id e2
    (EMul a e1 op e2) -> usesVar id e1 || usesVar id e2
    (ENot a e1)       -> usesVar id e1
    (EVar a id')      -> id == id'
    (EUVar a _)       -> False
    (EConst a c)      -> False

-- | Checks if the argument identifier appears in the argument pattern.
usesVarPat
    :: Ident  -- ^ Argument identifier, e.g x
    -> Pat a  -- ^ Argument pattern, e.g (x,3)
    -> Bool
usesVarPat id p = case p of
    PConst _ c       -> False
    PVar _ id'       -> id == id'
    PZAdt _ _        -> False
    PNAdt _ _ pats   -> any (usesVarPat id) pats
    PWild _          -> False
    PNil _           -> False
    PTup _ patterns  -> any (usesVarPat id) patterns
    PLay _ id' pat   -> id == id' || usesVarPat id pat

{-- | Checks if the argument identifier appears in the pattern match clause,
which is just a pair of a pattern and an expression.
-}
usesVarPatMatch :: Ident -> PatMatch a -> Bool
usesVarPatMatch id (PM pat exp) = usesVarPat id pat || usesVar id exp



{- The following instances of the Substitution typeclass are defined so that when
typechecking has completed and a valid substitution have (hopefully) been found, it
can be applied to the annotated definitions. During typechecking the program is annotated
with the fresh type variables that were generated for unification, and after unification
those type variables can be substituted for the actual type that has been inferred for
them.

It is not a faithful instance, as we do not implement the free type variables function.
We would probably have been better off creating a function
annotate :: Def Type -> Subst -> Def Type
that would perform the substitution for us.
-}



-- these instances are meant to be used when applying the inferred substitution to the
-- type annotated AST produced while typechecking.
instance Substitutable a => Substitutable (Def a) where
  apply s (DEquation t ident pats exp) = DEquation (apply s t) ident (apply s pats) (apply s exp)
  apply s (DMutRec tydefs) = DMutRec $ map (\(ty, defs) -> ((apply s ty), (map (apply s) defs))) tydefs
  apply _ d = d

  ftv d = undefined -- see below

instance Substitutable a => Substitutable (Pat a) where
  apply s (PConst a c)       = PConst (apply s a) c
  apply s (PVar a id)        = PVar (apply s a) id
  apply s (PZAdt a uid)      = PZAdt (apply s a) uid
  apply s (PNAdt a uid pats) = PNAdt (apply s a) uid (apply s pats)
  apply s (PWild a)          = PWild (apply s a)
  apply s (PNil a)           = PNil (apply s a)
  apply s (PTup a pats)      = PTup (apply s a) (apply s pats)
  apply s (PLay a id p)      = PLay (apply s a) id (apply s p)

  ftv p = undefined -- don't think we need this.. TODO backtrack here,
                    -- either implement for good measure or think of something else

instance Substitutable a => Substitutable (Exp a) where
  apply s (ECase a e branches) = ECase (apply s a) (apply s e) (map (apply s) branches)
  apply s (ELet a p e1 e2)     = ELet (apply s a) (apply s p) (apply s e1) (apply s e2)
  apply s (ELetR a p e1 e2)    = ELetR (apply s a) (apply s p) (apply s e1) (apply s e2)
  apply s (ELam a p e)         = ELam (apply s a) (apply s p) (apply s e)
  apply s (EIf a e1 e2 e3)     = EIf (apply s a) (apply s e1) (apply s e2) (apply s e3)
  apply s (EApp a e1 e2)       = EApp (apply s a) (apply s e1) (apply s e2)
  apply s (EOr a e1 e2)        = EOr (apply s a) (apply s e1) (apply s e2)
  apply s (EAnd a e1 e2)       = EAnd (apply s a) (apply s e1) (apply s e2)
  apply s (ERel a e1 op e2)    = ERel (apply s a) (apply s e1) (apply s op) (apply s e2)
  apply s (EAdd a e1 op e2)    = EAdd (apply s a) (apply s e1) (apply s op) (apply s e2)
  apply s (EMul a e1 op e2)    = EMul (apply s a) (apply s e1) (apply s op) (apply s e2)
  apply s (ETup a texps)       = ETup (apply s a) (map (apply s) texps)
  apply s (ENot a e)           = ENot (apply s a) (apply s e)
  apply s (EVar a id)          = EVar (apply s a) id
  apply s (EUVar a uid)        = EUVar (apply s a) uid
  apply s (EConst a c)         = EConst (apply s a) c

  ftv p = undefined -- same reasoning as above

instance Substitutable a => Substitutable (PatMatch a) where
  apply s (PM p e) = PM (apply s p) (apply s e)
  ftv p = undefined -- same reasoning as above

instance Substitutable a => Substitutable (AddOp a) where
  apply s (Plus a) = Plus (apply s a)
  apply s (Minus a) = Minus (apply s a)

  ftv p = undefined -- same reasoning as above

instance Substitutable a => Substitutable (MulOp a) where
  apply s (Times a) = Times (apply s a)
  apply s (Div a) = Div (apply s a)

  ftv p = undefined -- same reasoning as above

instance Substitutable a => Substitutable (RelOp a) where
  apply s (LTC a) = LTC (apply s a)
  apply s (LEC a) = LEC (apply s a)
  apply s (GTC a) = GTC (apply s a)
  apply s (GEC a) = GEC (apply s a)
  apply s (EQC a) = EQC (apply s a)

  ftv p = undefined -- same reasoning as above
