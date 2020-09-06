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
module Typechecker.AstUtils(
    getExpvar
  , isMoreGeneral

  , getPatType
  , getExpType
  , getPMType
  , getAddopVar
  , getMulopVar
  , getRelopVar

  , usesVar

  , function_type
  , unwrap_function
  , count_arguments

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

-- TODO make heavy use of this bad boy.... it would make
-- the inference code easier to read, I believe
infixr 8 *->
(*->) :: Type -> Type -> Type
t1 *-> t2 = TLam t1 t2

getExpvar :: Exp a -> a
getExpvar e = case e of
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

getPatvar :: Pat a -> a
getPatvar p = case p of
    PConst a _   -> a
    PVar a _     -> a
    PZAdt a _    -> a
    PNAdt a _ _  -> a
    PWild a      -> a
    PNil a       -> a
    PTup a _     -> a
    PLay a _ _   -> a

getAddopVar :: AddOp a -> a
getAddopVar op = case op of
    Plus a  -> a
    Minus a -> a

getMulopVar :: MulOp a -> a
getMulopVar op = case op of
  Times a -> a
  Div a   -> a

getRelopVar :: RelOp a -> a
getRelopVar op = case op of
  LTC a -> a
  LEC a -> a
  GTC a -> a
  GEC a -> a
  EQC a -> a

    -- Returns Just true if the first operand is of a more general type than
-- the type of the second operand.
-- TODO rewrite this to make the behaviour more specified perhaps.
-- Looking back, I am slightly confused myself as to what the returntype actually means.
isMoreGeneral :: Type -> Type -> Maybe Bool
isMoreGeneral (TLam t1 t1s) (TLam t2 t2s)     = (||) <$> 
                                                  isMoreGeneral t1 t2 <*> 
                                                  isMoreGeneral t1s t2s
isMoreGeneral (TVar _) (TVar _)               = Just False
isMoreGeneral (TVar _) _                      = Just True
isMoreGeneral (TAdt con1 t1s) (TAdt con2 t2s) =
    -- there is surely something built in for this
    let maybes = zipWith isMoreGeneral t1s t2s
        f (Just x) (Just y) = Just (x || y)
        f Nothing  _        = Nothing
        f _        Nothing  = Nothing
    in foldl f (Just False) maybes
isMoreGeneral (TTup t1s) (TTup t2s)           = 
    let maybes = zipWith isMoreGeneral t1s t2s
        f (Just x) (Just y) = Just (x || y)
        f Nothing  _        = Nothing
        f _        Nothing  = Nothing
    in foldl f (Just False) maybes
isMoreGeneral _ _                                 = Nothing

getPatType :: Pat Type -> Type
getPatType = getPatvar
--getPatType (PTyped _ _ t) = t
--getPatType _ = error "please don't end up here" -- TODO backtrack and take care of this

getExpType :: Exp Type -> Type
getExpType = getExpvar
--getExpType (ETyped _ _ t) = t
--getExpType _ = error "please don't end up here" -- TODO backtrack and take care of this

getPMType :: PatMatch Type -> Type
getPMType (PM _ e) = getExpvar e

-- builds a function type from the list of argument types and the result type
function_type :: [Type] -> Type -> Type
function_type [] res     = res
function_type (x:xs) res = TLam x (function_type xs res)

-- fetch the final construction of a type
-- e.g unwrap function (a -> b -> Either a b) = Either a b
unwrap_function :: Type -> Type
unwrap_function (TLam _ t) = unwrap_function t
unwrap_function t             = t

count_arguments :: Type -> Int
count_arguments (TLam _ t) = 1 + count_arguments t
count_arguments _             = 0

-- synonyms for the built-in types
bool :: Type
bool = TBool --TAdt () (UIdent "Bool") []

int :: Type
int = TInt --TAdt () (UIdent "Int") []

float :: Type
float = TFloat -- TAdt () (UIdent "Float") []

-- if _any_ of the expressions in the AST fulfils the predicate, return true
usesVar :: Ident -> Exp a -> Bool
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

usesVarPat :: Ident -> Pat a -> Bool
usesVarPat id p = case p of
    PConst _ c       -> False
    PVar _ id'       -> id == id'
    PZAdt _ _        -> False
    PNAdt _ _ pats   -> any (usesVarPat id) pats
    PWild _          -> False
    PNil _           -> False
    PTup _ patterns  -> any (usesVarPat id) patterns
    PLay _ id' pat   -> id == id' || usesVarPat id pat

usesVarPatMatch :: Ident -> PatMatch a -> Bool
usesVarPatMatch id (PM pat exp) = usesVarPat id pat || usesVar id exp

{- Substitution instances for backtracking -}

-- these instances are meant to be used when applying the inferred substitution to the
-- type annotated AST produced while typechecking.
instance Substitutable a => Substitutable (Def a) where
  apply s (DEquation t ident pats exp) = DEquation (apply s t) ident (apply s pats) (apply s exp)
  apply _ d = d

  ftv d = undefined -- see below

instance Substitutable a => Substitutable (Pat a) where
  --apply s (PTyped a p t)        = PTyped a (apply s p) (apply s t)
  apply s (PNAdt a con adtpats) = PNAdt (apply s a) con (map (apply s) adtpats)
  apply s (PTup a tuppats)      = PTup (apply s a) (map (apply s) tuppats)
  apply s (PLay a var pat)      = PLay (apply s a) var (apply s pat)
  apply s p                     = p

  ftv p = undefined -- don't think we need this.. TODO backtrack here,
                    -- either implement for good measure or think of something else

instance Substitutable a => Substitutable (Exp a) where
  --apply s (ETyped a e t) = ETyped a (apply s e) (apply s t)
  apply s (ETup a texps) = ETup (apply s a) (map (apply s) texps)
  apply s (ECase a e branches) = ECase (apply s a) (apply s e) (map (apply s) branches)
  apply s (ELet a p e1 e2) = ELet (apply s a) (apply s p) (apply s e1) (apply s e2)
  apply s (ELetR a p e1 e2) = ELetR (apply s a) (apply s p) (apply s e1) (apply s e2)
  apply s (ELam a p e)   = ELam (apply s a) (apply s p) (apply s e)
  apply s (EIf a e1 e2 e3)  = EIf (apply s a) (apply s e1) (apply s e2) (apply s e3)
  apply s (EApp a e1 e2)   = EApp (apply s a) (apply s e1) (apply s e2)
  apply s (EOr a e1 e2)    = EOr (apply s a) (apply s e1) (apply s e2)
  apply s (EAnd a e1 e2)   = EAnd (apply s a) (apply s e1) (apply s e2)
  apply s (ERel a e1 op e2) = ERel (apply s a) (apply s e1) (apply s op) (apply s e2)
  apply s (EAdd a e1 op e2) = EAdd (apply s a) (apply s e1) (apply s op) (apply s e2)
  apply s (EMul a e1 op e2) = EMul (apply s a) (apply s e1) (apply s op) (apply s e2)
  apply s (ENot a e)     = ENot (apply s a) (apply s e)
  apply s e = e

  ftv p = undefined -- same reasoning as above

instance Substitutable a => Substitutable (PatMatch a) where
  apply s (PM p e) = PM (apply s p) (apply s e)
  ftv p = undefined -- same reasoning as above

instance Substitutable a => Substitutable (AddOp a) where
  apply s (Plus a) = Plus (apply s a)
  apply s (Minus a) = Minus (apply s a)
  --apply s (AddOpTyped a op t) = AddOpTyped a op (apply s t)
  ftv p = undefined -- same reasoning as above

instance Substitutable a => Substitutable (MulOp a) where
  apply s (Times a) = Times (apply s a)
  apply s (Div a) = Div (apply s a)
  --apply s (MulOpTyped a op t) = MulOpTyped a op (apply s t)
  ftv p = undefined -- same reasoning as above

instance Substitutable a => Substitutable (RelOp a) where
  apply s (LTC a) = LTC (apply s a)
  apply s (LEC a) = LEC (apply s a)
  apply s (GTC a) = GTC (apply s a)
  apply s (GEC a) = GEC (apply s a)
  apply s (EQC a) = EQC (apply s a)
  --apply s (RelOpTyped a op t) = RelOpTyped a op (apply s t)
  ftv p = undefined -- same reasoning as above