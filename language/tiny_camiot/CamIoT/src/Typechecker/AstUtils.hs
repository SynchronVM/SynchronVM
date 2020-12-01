-- MIT License

-- Copyright (c) 2020 Robert Krook

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
         -- * Fetch functor values
         getExpVar
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

         -- * Aliases for AST types
       , (*->)
       , bool
       , int
       , float
       ) where

import Parser.AbsTinyCamiot
    ( PatMatch(..),
      Pat(..),
      RelOp(..),
      MulOp(Div, Times),
      AddOp(Minus, Plus),
      Exp(..),
      Type(TFloat, TVar, TAdt, TTup, TLam, TBool, TInt),
      Def(DEquation),
      Ident )
import Typechecker.Substitution ( Substitutable(..) )


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