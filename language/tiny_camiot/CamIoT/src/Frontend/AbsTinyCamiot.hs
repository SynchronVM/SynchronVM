-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AbsTinyCamiot where

import Prelude (Char, Double, Integer, String, map, fmap)
import qualified Prelude as C (Eq, Ord, Show, Read, Functor)
import qualified Data.String

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

newtype UIdent = UIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

data Def a
    = DEquation a Ident [Pat a] (Exp a)
    | DTypeSig a Ident (Type a)
    | DDataDec a UIdent [Ident] [ConstructorDec a]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor Def where
    fmap f x = case x of
        DEquation a ident pats exp -> DEquation (f a) ident (map (fmap f) pats) (fmap f exp)
        DTypeSig a ident type_ -> DTypeSig (f a) ident (fmap f type_)
        DDataDec a uident idents constructordecs -> DDataDec (f a) uident idents (map (fmap f) constructordecs)

data ConstructorDec a = ConstDec a UIdent (Type a)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor ConstructorDec where
    fmap f x = case x of
        ConstDec a uident type_ -> ConstDec (f a) uident (fmap f type_)

data Type a
    = TLam a (Type a) (Type a)
    | TVar a Ident
    | TNil a
    | TAdt a UIdent [Type a]
    | TTup a [TupType a]
    | TInt a
    | TFloat a
    | TBool a
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor Type where
    fmap f x = case x of
        TLam a type_1 type_2 -> TLam (f a) (fmap f type_1) (fmap f type_2)
        TVar a ident -> TVar (f a) ident
        TNil a -> TNil (f a)
        TAdt a uident types -> TAdt (f a) uident (map (fmap f) types)
        TTup a tuptypes -> TTup (f a) (map (fmap f) tuptypes)
        TInt a -> TInt (f a)
        TFloat a -> TFloat (f a)
        TBool a -> TBool (f a)

data TupType a = TTupType a (Type a)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor TupType where
    fmap f x = case x of
        TTupType a type_ -> TTupType (f a) (fmap f type_)

data TupExp a = ETupExp a (Exp a)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor TupExp where
    fmap f x = case x of
        ETupExp a exp -> ETupExp (f a) (fmap f exp)

data Exp a
    = ECase a (Exp a) [PatMatch a]
    | ELet a (Pat a) (Exp a) (Exp a)
    | ELetR a (Pat a) (Exp a) (Exp a)
    | ELam a (Pat a) (Exp a)
    | EIf a (Exp a) (Exp a) (Exp a)
    | EApp a (Exp a) (Exp a)
    | EOr a (Exp a) (Exp a)
    | EAnd a (Exp a) (Exp a)
    | ERel a (Exp a) (RelOp a) (Exp a)
    | EAdd a (Exp a) (AddOp a) (Exp a)
    | EMul a (Exp a) (MulOp a) (Exp a)
    | ENot a (Exp a)
    | ETup a [TupExp a]
    | EUVar a UIdent
    | EVar a Ident
    | EConst a (Const a)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor Exp where
    fmap f x = case x of
        ECase a exp patmatchs -> ECase (f a) (fmap f exp) (map (fmap f) patmatchs)
        ELet a pat exp1 exp2 -> ELet (f a) (fmap f pat) (fmap f exp1) (fmap f exp2)
        ELetR a pat exp1 exp2 -> ELetR (f a) (fmap f pat) (fmap f exp1) (fmap f exp2)
        ELam a pat exp -> ELam (f a) (fmap f pat) (fmap f exp)
        EIf a exp1 exp2 exp3 -> EIf (f a) (fmap f exp1) (fmap f exp2) (fmap f exp3)
        EApp a exp1 exp2 -> EApp (f a) (fmap f exp1) (fmap f exp2)
        EOr a exp1 exp2 -> EOr (f a) (fmap f exp1) (fmap f exp2)
        EAnd a exp1 exp2 -> EAnd (f a) (fmap f exp1) (fmap f exp2)
        ERel a exp1 relop exp2 -> ERel (f a) (fmap f exp1) (fmap f relop) (fmap f exp2)
        EAdd a exp1 addop exp2 -> EAdd (f a) (fmap f exp1) (fmap f addop) (fmap f exp2)
        EMul a exp1 mulop exp2 -> EMul (f a) (fmap f exp1) (fmap f mulop) (fmap f exp2)
        ENot a exp -> ENot (f a) (fmap f exp)
        ETup a tupexps -> ETup (f a) (map (fmap f) tupexps)
        EUVar a uident -> EUVar (f a) uident
        EVar a ident -> EVar (f a) ident
        EConst a const -> EConst (f a) (fmap f const)

data AddOp a = Plus a | FPlus a | Minus a | FMinus a
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor AddOp where
    fmap f x = case x of
        Plus a -> Plus (f a)
        FPlus a -> FPlus (f a)
        Minus a -> Minus (f a)
        FMinus a -> FMinus (f a)

data MulOp a = Times a | FTImes a | Div a | FDiv a
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor MulOp where
    fmap f x = case x of
        Times a -> Times (f a)
        FTImes a -> FTImes (f a)
        Div a -> Div (f a)
        FDiv a -> FDiv (f a)

data RelOp a
    = LTC a
    | FLTC a
    | LEC a
    | FLEC a
    | GTC a
    | FGTC a
    | GEC a
    | FGEC a
    | EQC a
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor RelOp where
    fmap f x = case x of
        LTC a -> LTC (f a)
        FLTC a -> FLTC (f a)
        LEC a -> LEC (f a)
        FLEC a -> FLEC (f a)
        GTC a -> GTC (f a)
        FGTC a -> FGTC (f a)
        GEC a -> GEC (f a)
        FGEC a -> FGEC (f a)
        EQC a -> EQC (f a)

data Con a = Constructor a UIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor Con where
    fmap f x = case x of
        Constructor a uident -> Constructor (f a) uident

data Const a
    = CInt a Integer | CFloat a Double | CTrue a | CFalse a | CNil a
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor Const where
    fmap f x = case x of
        CInt a integer -> CInt (f a) integer
        CFloat a double -> CFloat (f a) double
        CTrue a -> CTrue (f a)
        CFalse a -> CFalse (f a)
        CNil a -> CNil (f a)

data Pat a
    = PConst a (Const a)
    | PVar a Ident
    | PZAdt a UIdent
    | PNAdt a UIdent [AdtPat a]
    | PWild a
    | PNil a
    | PTup a [TupPat a]
    | PLay a Ident (Pat a)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor Pat where
    fmap f x = case x of
        PConst a const -> PConst (f a) (fmap f const)
        PVar a ident -> PVar (f a) ident
        PZAdt a uident -> PZAdt (f a) uident
        PNAdt a uident adtpats -> PNAdt (f a) uident (map (fmap f) adtpats)
        PWild a -> PWild (f a)
        PNil a -> PNil (f a)
        PTup a tuppats -> PTup (f a) (map (fmap f) tuppats)
        PLay a ident pat -> PLay (f a) ident (fmap f pat)

data AdtPat a = PAdtPat a (Pat a)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor AdtPat where
    fmap f x = case x of
        PAdtPat a pat -> PAdtPat (f a) (fmap f pat)

data TupPat a = PTupPat a (Pat a)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor TupPat where
    fmap f x = case x of
        PTupPat a pat -> PTupPat (f a) (fmap f pat)

data PatMatch a = PM a (Pat a) (Exp a)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

instance C.Functor PatMatch where
    fmap f x = case x of
        PM a pat exp -> PM (f a) (fmap f pat) (fmap f exp)

