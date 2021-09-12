{- | This module implements a pretty-printer for the syntax defined in
"CamIoT.Internal.Syntax". It is, in large part, stolen from what BNFC generates. -}
{-# LANGUAGE FlexibleInstances #-}
module CamIoT.Pretty.Syntax where

import CamIoT.Internal.Syntax

import Data.Char
import Data.Maybe

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print () where
  prt _ x = doc (shows x)

instance Print Int where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Bool where
  prt _ x = doc (shows x)

instance Print Ident where
  prt _ (Ident i) = doc $ showString i
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print UIdent where
  prt _ (UIdent i) = doc $ showString i

instance Print a => Print [Def a] where
  prt = prtList

instance Print a => Print (Def a) where
  prt i e = case e of
    DEquation a id pats exp -> prPrec i 0 (concatD [prt 0 id, prt 0 pats, doc (showString "="), prt 0 exp, doc (showString ":"), prt 0 a])
    DTypeSig id type_ -> prPrec i 0 (concatD [prt 0 id, doc (showString ":"), prt 0 type_])
    DDataDec uident ids constructordecs -> prPrec i 0 (concatD [doc (showString "data"), prt 0 uident, prt 0 ids, doc (showString "where"), doc (showString "{"), prt 0 constructordecs, doc (showString "}")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print (UIdent, Type) where
  prt i (uid,t) = prPrec i 0 (concatD [prt 0 uid, doc (showString ":"), prt 0 t])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [(UIdent, Type)] where
  prt = prtList

instance Print [Ident] where
  prt = prtList

instance Print Type where
  prt i e = case e of
    TLam type_1 type_2 -> prPrec i 0 (concatD [prt 1 type_1, doc (showString "->"), prt 0 type_2])
    TVar id -> prPrec i 1 (concatD [prt 0 id])
    TTup ts -> prPrec i 1 (concatD ([doc (showString "(")] ++ printTups ts ++ [doc (showString ")")]))
    TInt -> prPrec i 2 (concatD [doc (showString "Int")])
    TFloat -> prPrec i 2 (concatD [doc (showString "Float")])
    TBool -> prPrec i 2 (concatD [doc (showString "Bool")])
    TNil -> prPrec i 2 (concatD [doc (showString "()")])
    TAdt uident types -> prPrec i 2 (concatD [prt 0 uident, prt 1 types])
    where
        printTups [] = []
        printTups [x] = [prt 0 x]
        printTups (x:y:xs) = [prt 0 x, doc (showString ",")] ++ printTups (y:xs) 
  prtList n [] = concatD []
  prtList n (x:xs) = concatD [prt n x, prt n xs]


instance Print [Type] where
  prt = prtList

instance Print a => Print (Exp a) where
  prt i e = case e of
    ELet a pat exp1 exp2 -> prPrec i 0 (concatD [doc (showString "let"), prt 0 pat, doc (showString "="), prt 0 exp1, doc (showString "in"), prt 0 exp2])
    ELam a pat exp -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 pat, doc (showString "->"), prt 0 exp])
    EApp a exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, prt 7 exp2])
    EBin a exp1 exp2 o -> prPrec i 4 (concatD [prt 4 exp1, prt 4 o, prt 5 exp2])
    EUn a e o -> prPrec i 4 (concatD [prt 4 o, prt 4 e])
    ETup a es -> prPrec i 7 (concatD ([doc (showString "(")] ++ printTups es ++ [doc (showString ")")]))
    EVar a id -> prPrec i 7 (concatD [prt 0 id])
    ECon a uid -> prPrec i 7 (concatD [prt 0 uid])
    ELit a const -> prPrec i 7 (concatD [prt 0 const])
    ECase a exp patmatchs -> prPrec i 0 (concatD [doc (showString "case"), prt 0 exp, doc (showString "of"), doc (showString "{"), prt 0 patmatchs, doc (showString "}")])
    EIf a exp1 exp2 exp3 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 exp1, doc (showString "then"), prt 0 exp2, doc (showString "else"), prt 0 exp3])
    where
        printTups [] = []
        printTups [x] = [prt 0 x]
        printTups (x:y:xs) = [prt 0 x, doc (showString ",")] ++ printTups (y:xs)

  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print a => Print (Pat a, Exp a) where
  prt i (pat,exp) = prPrec i 0 (concatD [prt 0 pat, doc (showString "->"), prt 0 exp])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print a => Print [Exp a] where
  prt = prtList

instance Print a => Print (Binop a) where
  prt i op = case op of
    Add a -> prPrec i 4 (concatD [doc (showString "+")])
    Sub a -> prPrec i 2 (concatD [doc (showString "-")])
    Mul a -> prPrec i 2 (concatD [doc (showString "*")])
    Div a -> prPrec i 2 (concatD [doc (showString "/")])
    OLT a -> prPrec i 2 (concatD [doc (showString "<")])
    OLE a -> prPrec i 2 (concatD [doc (showString "<=")])
    OGT a -> prPrec i 2 (concatD [doc (showString ">")])
    OGE a -> prPrec i 2 (concatD [doc (showString ">=")])
    OEQ a -> prPrec i 2 (concatD [doc (showString "==")])
    And a -> prPrec i 2 (concatD [doc (showString "&&")])
    Or a  -> prPrec i 2 (concatD [doc (showString "||")])

instance Print a => Print (Unop a) where
  prt i op = case op of
    Not a -> prPrec i 2 (concatD [doc (showString "!")])

instance Print Lit where
  prt i e = case e of
    LInt n -> prPrec i 0 (concatD [prt 0 n])
    LFloat n -> prPrec i 0 (concatD [prt 0 n])
    LBool n -> prPrec i 0 (concatD [prt 0 n])
    LNil -> prPrec i 0 (concatD [doc (showString "()")])

instance Print (Pat a) where
  prt i e = case e of
    PConst _ const -> prPrec i 0 (concatD [prt 0 const])
    PVar _ id -> prPrec i 0 (concatD [prt 0 id])
    PNil _ -> prPrec i 0 (concatD [doc (showString "()")])
    PWild _ -> prPrec i 0 (concatD [doc (showString "_")])
    PAs _ id pat -> prPrec i 2 (concatD [prt 0 id, doc (showString "as"), prt 0 pat])
    PAdt _ uident [] -> prPrec i 0 (concatD [prt 0 uident])
    PAdt _ uident adtpats -> prPrec i 0 (concatD [doc (showString "("), prt 0 uident, prt 0 adtpats, doc (showString ")")])
    PTup _ ps -> prPrec i 1 (concatD $ [doc (showString "(")] ++ printTups ps ++ [doc (showString ")")])
    where
        printTups [] = []
        printTups [x] = [prt 0 x]
        printTups (x:y:xs) = [prt 0 x, doc (showString ",")] ++ printTups (y:xs) 
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Pat a] where
  prt = prtList

instance Print a => Print (Function a) where
--  prt i f = prtList 0 $ concat [maybe [] (\t -> [DTypeSig (name f) t]) (typesig f), equations f]
  prt i f = concatD (tsig:defs)
    where
       tsig = concatD [ prt 0 (name f)
                      , doc (showString ":")
                      , prt 0 $ fromJust $ typesig f
                      , doc (showString ";")
                      ]
       defs = (flip map) (equations f) $ \(args, body) ->
         concatD [ prt 0 (name f)
                 , prtList 0 args
                 , doc (showString "=")
                 , prt 0 body
                 , doc (showString ";")
                 ]
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print a => Print (Program a) where
  prt i p = concatD [prtList 0 datadecs, prtList 0 (functions p ++ [main p])]
    where
        datadecs :: [Def ()]
        datadecs = map (\(uid, vars, cons) -> DDataDec uid vars cons) (datatypes p)
