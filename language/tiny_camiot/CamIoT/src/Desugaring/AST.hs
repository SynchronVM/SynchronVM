{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Desugaring.AST where

import qualified Parser.AbsTinyCamiot as AST
import Parser.PrintTinyCamiot

import Data.Char

data SType
    = STLam SType SType
    | STNil
    | STAdt AST.UIdent
    | STTup SType SType
    | STBool
    | STInt
    | STFloat
  deriving (Eq, Ord, Show, Read)

data SExp a
    = SECase  a (SExp a) [SPatMatch a]
    | SELet   a (SPat a) (SExp a) (SExp a)
    | SELetR  a (SPat a) (SExp a) (SExp a)
    | SELam   a (SPat a) (SExp a)
    | SEIf    a (SExp a) (SExp a) (SExp a)
    | SEApp   a (SExp a) (SExp a)
    | SEOr    a (SExp a) (SExp a)
    | SEAnd   a (SExp a) (SExp a)
    | SERel   a (SExp a) (AST.RelOp a) (SExp a)
    | SEAdd   a (SExp a) (AST.AddOp a) (SExp a)
    | SEMul   a (SExp a) (AST.MulOp a) (SExp a)
    | SETup   a (SExp a) (SExp a)
    | SENot   a (SExp a)
    | SEVar   a AST.Ident
    | SEUVar  a AST.UIdent
    | SEConst a AST.Const
  deriving (Eq, Ord, Show, Read)

data SPatMatch a = SPM (SPat a) (SExp a)
  deriving (Eq, Ord, Show, Read)

getSExpVar :: SExp a -> a
getSExpVar e = case e of
    SECase  a _ _   -> a
    SELet   a _ _ _ -> a
    SELetR  a _ _ _ -> a
    SELam   a _ _   -> a
    SEIf    a _ _ _ -> a
    SEApp   a _ _   -> a
    SEOr    a _ _   -> a
    SEAnd   a _ _   -> a
    SERel   a _ _ _ -> a
    SEAdd   a _ _ _ -> a
    SEMul   a _ _ _ -> a
    SETup   a _ _   -> a
    SENot   a _     -> a
    SEVar   a _     -> a
    SEUVar  a _     -> a
    SEConst a _     -> a

data SPat a
    = SPConst a AST.Const
    | SPVar a AST.Ident
    | SPNAdt a AST.UIdent [SPat a]
    | SPWild a
    | SPNil a
    | SPTup a (SPat a) (SPat a)
    | SPLay a AST.Ident (SPat a)
  deriving (Eq, Ord, Show, Read)

{- ********** Pretty Printing ********** -}

-- | The top-level printing method.

instance Print SType where
  prt i e = case e of
    STLam type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])
    STNil -> prPrec i 2 (concatD [doc (showString "()")])
    STAdt uident -> prPrec i 2 (prt 0 uident)
    STTup t1 t2 -> prPrec i 1 (concatD ([doc (showString "(")] ++ printTups [t1, t2] ++ [doc (showString ")")]))
    STBool -> prPrec i 2 (concatD [doc (showString "Bool")])
    STInt -> prPrec i 2 (concatD [doc (showString "Int")])
    STFloat -> prPrec i 2 (concatD [doc (showString "Float")])
    where
        printTups [] = []
        printTups [x] = [prt 0 x]
        printTups (x:y:xs) = [prt 0 x, doc (showString ",")] ++ printTups (y:xs) 
  prtList n [] = concatD []
  prtList n (x:xs) = concatD [prt n x, prt n xs]


instance Print [SType] where
  prt = prtList

instance Print a => Print (SExp a) where
  prt i e = case e of
    SECase a exp patmatchs -> prPrec i 0 (concatD [doc (showString "case"), prt 0 exp, doc (showString "of"), doc (showString "{"), prt 0 patmatchs, doc (showString "}")])
    SELet a pat exp1 exp2 -> prPrec i 0 (concatD [doc (showString "let"), prt 0 pat, doc (showString "="), prt 0 exp1, doc (showString "in"), prt 0 exp2])
    SELetR a pat exp1 exp2 -> prPrec i 0 (concatD [doc (showString "letrec"), prt 0 pat, doc (showString "="), prt 0 exp1, doc (showString "in"), prt 0 exp2])
    SELam a pat exp -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 pat, doc (showString "->"), prt 0 exp])
    SEIf a exp1 exp2 exp3 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 exp1, doc (showString "then"), prt 0 exp2, doc (showString "else"), prt 0 exp3])
    SEApp a exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, prt 7 exp2])
    SEOr a exp1 exp2 -> prPrec i 1 (concatD [prt 2 exp1, doc (showString "||"), prt 1 exp2])
    SEAnd a exp1 exp2 -> prPrec i 2 (concatD [prt 3 exp1, doc (showString "&&"), prt 2 exp2])
    SERel a exp1 relop exp2 -> prPrec i 3 (concatD [prt 3 exp1, prt 0 relop, prt 4 exp2])
    SEAdd a exp1 addop exp2 -> prPrec i 4 (concatD [prt 4 exp1, prt 0 addop, prt 5 exp2])
    SEMul a exp1 mulop exp2 -> prPrec i 5 (concatD [prt 5 exp1, prt 0 mulop, prt 6 exp2])
    SETup a t1 t2 -> prPrec i 7 (concatD ([doc (showString "(")] ++ printTups [t1, t2] ++ [doc (showString ")")]))
    SENot a exp -> prPrec i 6 (concatD [doc (showString "!"), prt 7 exp])
    SEVar a id -> prPrec i 7 (concatD [prt 0 id])
    SEUVar a uident -> prPrec i 7 (concatD [prt 0 uident])
    SEConst a const -> prPrec i 7 (concatD [prt 0 const])
    where
        printTups [] = []
        printTups [x] = [prt 0 x]
        printTups (x:y:xs) = [prt 0 x, doc (showString ",")] ++ printTups (y:xs) 

  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]
    
instance Print a => Print [SExp a] where
  prt = prtList

instance Print (SPat a) where
  prt i e = case e of
    SPConst _ const -> prPrec i 0 (concatD [prt 0 const])
    SPVar _ id -> prPrec i 0 (concatD [prt 0 id])
    SPNAdt _ uident adtpats -> prPrec i 0 (concatD [doc (showString "("), prt 0 uident, prt 0 adtpats, doc (showString ")")])
    SPWild _ -> prPrec i 0 (concatD [doc (showString "_")])
    SPNil _ -> prPrec i 0 (concatD [doc (showString "("), doc (showString ")")])
    SPTup _ p1 p2 -> prPrec i 1 (concatD ([doc (showString "(")] ++ printTups [p1, p2]++ [doc (showString ")")]))
    SPLay _ id pat -> prPrec i 2 (concatD [prt 0 id, doc (showString "as"), prt 0 pat])
    where
        printTups [] = []
        printTups [x] = [prt 0 x]
        printTups (x:y:xs) = [prt 0 x, doc (showString ",")] ++ printTups (y:xs)
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [SPat a] where
  prt = prtList

instance Print a => Print (SPatMatch a) where
  prt i e = case e of
    SPM pat exp -> prPrec i 0 (concatD [prt 0 pat, doc (showString "->"), prt 0 exp])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print a => Print [SPatMatch a] where
  prt = prtList