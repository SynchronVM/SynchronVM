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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Bytecode where

import Control.Monad.State.Class
import Control.Monad.State.Strict
import Data.Int(Int32)
import Data.List
import Desugaring.AST
import GHC.Float(double2Float)
import Interpreter.Interpreter
import Lib

import qualified CAM as C
import qualified Assembler as A
import qualified Parser.AbsTinyCamiot as AST
import qualified Parser.PrintTinyCamiot as PP
import qualified Bytecode.InterpreterModel as IM

translate :: SExp SType -> C.Exp
translate (SEIf _ cond thn els) =
  C.If (translate cond) (translate thn) (translate els)
translate (SETup _ e1 e2) =
  C.Pair (translate e1) (translate e2)
translate (SENot _ e) =
  C.Sys $ C.Sys1 C.NOT (translate e)
translate (SEVar _ (AST.Ident ident)) =
  C.Var ident
translate (SETag _ (AST.UIdent tag) maybeExp) =
  case maybeExp of
    Nothing -> C.Con tag C.Void
    Just e  -> C.Con tag (translate e)
translate (SEApp _ e1 e2) = C.App (translate e1) (translate e2)
translate (SERel _ e1 relop e2) =
  case relop of
    AST.LTC _ ->
      C.Sys $ C.Sys2 (genlt (typeof e1)) (translate e1) (translate e2)
    AST.LEC _ ->
      C.Sys $ C.Sys2 (genle (typeof e1)) (translate e1) (translate e2)
    AST.GTC _ ->
      C.Sys $ C.Sys2 (gengt (typeof e1)) (translate e1) (translate e2)
    AST.GEC _ ->
      C.Sys $ C.Sys2 (genge (typeof e1)) (translate e1) (translate e2)
    AST.EQC _ ->
      C.Sys $ C.Sys2 (geneq (typeof e1)) (translate e1) (translate e2)
  where
    genlt STInt = C.BLT
    genlt _     = error "LT for other types not yet supported"
    genle STInt = C.BLE
    genle _     = error "LE for other types not yet supported"
    gengt STInt = C.BGT
    gengt _     = error "GT for other types not yet supported"
    genge STInt = C.BGE
    genge _     = error "GE for other types not yet supported"
    geneq STInt = C.BEQ
    geneq _     = error "EQ for other types not yet supported"

translate (SEAdd ty e1 _ e2) =
  let e1' = translate e1
      e2' = translate e2
   in C.Sys $ C.Sys2 addOp e1' e2'
   where
     addOp = case ty of
               STInt   -> C.PlusI
               STFloat -> C.PlusF
               _ -> error "Adding non int or float operation"
translate (SEMul ty e1 _ e2) =
  let e1' = translate e1
      e2' = translate e2
   in C.Sys $ C.Sys2 mulOp e1' e2'
   where
     mulOp = case ty of
               STInt   -> C.MultiplyI
               STFloat -> C.MultiplyF
               _ -> error "Multiplying non int or float operation"

translate (SEConst ty const) =
  case const of
    AST.CInt arbitraryPrecisionInt ->
      C.Sys (C.LInt (fromInteger arbitraryPrecisionInt)) -- only 32 bit Ints supported in CAM currently
    AST.CFloat double ->
      C.Sys (C.LFloat (double2Float double)) -- only single precision floats supported in CAM currently
    AST.CTrue  -> C.Sys (C.LBool True)
    AST.CFalse -> C.Sys (C.LBool False)
    AST.CNil   -> C.Void

-- Exprs containing rich patterns which need to be rewritten
-- Case, Let, LetR, Lam
translate e@(SELet _ pat e1 e2)
  | rewriteRequired pat = evalState (runCodegen $ rewriteLet e) (initState 0)
  | (SPConst _ _) <- pat = -- See NOTE 1 below to understand why we did this
      error "Constants in the left side of a let expression not supported"
  | otherwise =  C.Let (translatePat pat) (translate e1) (translate e2)

translate (SELetR _ pat e1 e2)
  | rewriteRequired pat = undefined
  | (SPConst _ _) <- pat = -- See NOTE 1 below to understand why we did this
      error "Constants in the left side of a letrec expression not supported"
  | otherwise =  C.Letrec [((translatePat pat),(translate e1))] (translate e2)

translate (SELam _ pat exp)
  | rewriteRequired pat = undefined
  | otherwise =  C.Lam (translatePat pat) (translate exp)

-----------------REWRITES FOR CASE EXPRESSION-----------------

{-
(\ s ->
case s of
   x -> x) 5

-- REWRITE --

(\ s ->
   let x = s
    in x) 5

-}
translate (SECase _ cond [(SPM (SPVar _ (AST.Ident var)) exp)]) =
  C.Let (C.PatVar var) (translate cond) (translate exp)

{-
(\ s ->
case s of
   _ -> s) 5

-- REWRITE --

(\ s ->
   let _ = s
    in s) 5

-}

translate (SECase _ cond [(SPM (SPWild _) exp)]) =
  C.Let C.Empty (translate cond) (translate exp)

{-
(\ s ->
case s of
   (x,y) -> x + y) (5,6)
-- or any arbitrary tuple made of variables

-- REWRITE --

(\ s ->
   let (x,y) = s
    in x + y) (5,6)

-}

translate expr@(SECase _ cond [(SPM p@(SPTup _ p1 p2) exp)])
  | rewriteRequired p = rewriteTupleCases expr
  | otherwise = C.Let (translatePat p) (translate cond) (translate exp)

{-

More than one pattern match expression where
first entry is a tuple. Possible rewrite to an if
or case or a combination

-}
translate expr@(SECase _ _ ((SPM (SPTup _ _ _) _):_)) =
  rewriteTupleCases expr

-----------------REWRITES FOR CASE EXPRESSION ENDS-----------------

translate (SECase _ cond clauses) =
  C.Case (translate cond) (map translatePatMats clauses)

-- Not supported yet
translate (SEOr  _ e1 e2) = error "Native OR  not supported yet"
translate (SEAnd _ e1 e2) = error "Native AND not supported yet"


translatePatMats :: SPatMatch SType -> (C.TaggedField, C.Exp)
translatePatMats (SPM (SPVar _ (AST.Ident var)) expr) =
  ((wildcardtag, C.PatVar var), translate expr)
translatePatMats (SPM (SPWild _) expr) =
  ((wildcardtag, C.Empty), translate expr)
translatePatMats (SPM (SPNil _) expr) =
  ((wildcardtag, C.Empty), translate expr)
translatePatMats (SPM (SPNAdt _ (AST.UIdent tag) rest) expr) =
  case rest of
    Nothing -> ((tag, C.Empty), translate expr)
    Just x
      | rewriteRequired x -> undefined--  case cond of
                                      --       m:n:ys -> e;
                                      --  you are passed m:n:ys -> e part here
                                      --  m:n:ys is stored as
                                      --  (":"  Just  (Pair (PV m) (":" Just (Pair (PV n) (PV ys)))))
                                      --  tag   Just                   x
                                      --
                                      --  Hence rewritten to
                                      --  case cond of
                                      --        m:temp1 -> case temp1 of
                                      --                         n:ys -> e
                                      --  APPROACH1 frontend
                                      --   do
                                      --   temp1 <- genfresh
                                      --   let pat = (":" Just (Pair (PV m) (PV temp1)))
                                      --   let newExpr = SECase (typeof expr) (SEVar temp1)
                                      --                      [SPM (SPNAdt ":" (Just (Pair "n" "ys"))) expr] -- recursion here
                                      --   translatePatMats (SPM pat newExpr)
                                      --
                                      -- APPROACH2 middleware
                                      -- do
                                      -- temp1 <- genfresh
                                      -- ((tag = ":", C.Pair (PatVar "m") (PatVar "temp1")),
                                      --  (Case (Var "temp1") [((":", (C.Pair (PatVar n) (PatVar ys))), expr)]))
      | otherwise -> ((tag, translatePat x), translate expr)
translatePatMats p =
  error $ "Constants, tuples and @ expression should be rewritten. Error: "
        <> show p
wildcardtag = "??WILDCARD??"

rewriteRequired :: SPat SType -> Bool
rewriteRequired (SPLay _  _ pat) = rewriteRequired pat
rewriteRequired (SPTup _ p1 p2)  = rewriteRequired p1 || rewriteRequired p2
rewriteRequired (SPConst _ _)  = True
rewriteRequired (SPNAdt _ _ _) = True
rewriteRequired _ = False



translatePat :: SPat SType -> C.Pat
translatePat (SPNil _ ) = C.Empty
translatePat (SPVar _ (AST.Ident str)) = C.PatVar str
translatePat (SPWild _) = C.Empty
-- Tricky cases--
translatePat (SPLay _ (AST.Ident str) pat) = C.As str (translatePat pat)
translatePat (SPTup _ p1 p2)
  = C.PatPair (translatePat p1) (translatePat p2)
translatePat (SPConst _ const) = error "Rewrite constants has failed"
translatePat (SPNAdt _ _ _)    = error "Rewrite constructors has failed"



rewriteTupleCases :: SExp SType -> C.Exp
-- rewriteTupleCases (SECase _ cond ((SPM (SPTup _ (SPNAdt _ (AST.UIdent constr) rest) p2) expr):_)) = undefined
rewriteTupleCases (SECase _ _ ((SPM (SPTup _ _ _) _):_)) = error "Unhandled tuple case"
rewriteTupleCases _ = error "Not a tuple case"

rewriteLet :: SExp SType -> Codegen C.Exp

{-
let Nothing = e1
 in e2
-}
rewriteLet (SELet _ (SPNAdt _ (AST.UIdent constr) Nothing) ebound ein) =
  pure $ C.Case (translate ebound) [((constr, C.Empty), translate ein)]

{-
let m:n:ns = e1
 in e2

--REWRITTEN TO--

case e1 of
  m:temp0 -> rewriteLet (let n:ys = temp0
                        in e2)

Can support m:n:.....xs
-}

rewriteLet (SELet ty1 (SPNAdt _
                       (AST.UIdent constr1) -- :
                       (Just (SPTup _
                              (SPVar _ (AST.Ident var1)) -- m
                              (SPNAdt ty2
                               (AST.UIdent constr2) -- :
                               (Just pat))))) ebound ein) = do-- (n,ns)
  tempVar <- fresh
  currentCount <- gets count
  let newLetExpr = SELet
                   ty1
                   (SPNAdt ty2 (AST.UIdent constr2) (Just pat))
                   (SEVar ty2 (AST.Ident tempVar))
                   ein
  pure $
    C.Case
    (translate ebound)
    [((constr1, (C.PatPair (C.PatVar var1) (C.PatVar tempVar)))
     , evalState (runCodegen $ rewriteLet newLetExpr) (initState currentCount)
     )
    ]



{-

let Just x = e1
 in e2


let x:xs = e1
 in e2


let Just x/Just _ /Just (x,y) = e1
 in e2
-}
rewriteLet (SELet _ (SPNAdt _ (AST.UIdent constr) (Just pat)) ebound ein) =
  pure $ C.Case (translate ebound) [((constr, translatePat pat), translate ein)]



rewriteLet _ = error "Let not rewritten"





-- let (x:ys) = e1
-- in e2


-- let (m:n:ys) = e1
-- in e2

-- case e1 of
--   m:temp -> case temp of
--                 n:ys -> e2

-- let (SPNAdt ":" (Just (SPTup
--                        (SPVar m)
--                        (SPNAdt ":" (Just (SPTup
--                                           (SPVar n)
--                                           (SPVar ys)
--                                          )))))) = e1
-- in e2

-- case e1 of
--   [(":", PatPair m temp), case temp [((":", PatPair n ys), e2)]
-- ]


data CodegenState =
  CodegenState
    { count :: Word }
  deriving (Show)

initState i = CodegenState { count = i }

newtype Codegen a =
  Codegen
    { runCodegen :: State CodegenState a
    }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

fresh :: Codegen String
fresh = do
  i <- gets count
  modify $ \s -> s {count = 1 + i}
  return $ "temp_" <> (show i)


typeof = getSExpVar


{- NOTE 1

Consider this program:

foo = let 1 = 2
       in 1

which if rewritten would become

foo = (\1 -> 1) 2

which would give a different result from the above.

If rewritten in a different way:

foo = let x = 2 in
      if x == 1
      then 1
      else Nil

which is an error.

So DO NOT REWRITE let expressions with constant
patterns on the left of a let expression!

And CAM doesn't support constants in the left side
of let expressions.
-}




-- Experiments --
path = "testcases/good8.cam"

test :: IO ()
test = do
  compiled <- compile path
  case compiled of
    Left err -> putStrLn err
    Right desugaredIr -> do
      putStrLn $ PP.printTree desugaredIr
      --putStrLn $ show desugaredIr
      let camir = translate desugaredIr
      let cam   = C.interpret camir
      -- putStrLn $ show cam
      let val = IM.evaluate $ C.interpret camir
      putStrLn $ show val
      -- A.genbytecode cam
      -- putStrLn $ show $ A.translate $ C.interpret $ translate desugaredIr

foo =
  let v0 = \ v2 -> case v2 of
                     v1 -> v1 * 2
  in v0 5

bar = (\(x:xs) -> x) [1,2]
bar' = case [1,2] of
         (x:xs) -> x

baz = let (x:y:_, m:n:_) = ([1,2], [3,4])
       in (x + y + m + n)

baz' = case ([1,2], [3,4]) of
         (x:xs,m:ms) ->
           case (xs,ms) of
             (y:_, n:_) -> x + y + m + n

foo' = (\1 -> 1) 1

zoo :: Maybe [a] -> Maybe [a]
zoo x = case x of
          Nothing -> Nothing
          g@(Just [])     -> g
          f@(Just (x:xs)) -> f

quux x = case x of
           f@(1,2) -> fst f + snd f
           g@_     -> fst g * snd g
quux' x = let f = x
          in let g = x
             in let (temp1, temp2) = f
                in if temp1 == 1
                   then if temp2 == 2
                        then fst f + snd f
                        else undefined
                   else fst g * snd g

-- r x = case x of
--         (m:ms,2) -> m
-- r' x = let temp = x
--        in let (temp1, temp2) = temp
--           in case temp1 of
--                m:ms -> if temp2 == 2
--                        then m
--                        else undefined

grault x = case x of
             (1, _, 3)   -> 1
             (1,m:ms, _) -> m
             (2, _, g@_) -> g
             m           -> 5


grault' x = case x of
             (1, (_, 3))   -> 1
             (1,(m:ms, _)) -> m
             (2, (_, g@_)) -> g
             m             -> 5

-- grault'' x = let temp = x
--              in let (temp1, (temp2, temp3)) = temp
--                 in if temp1 == 1
--                    then case temp2 of
--                           _ -> if temp3 == 3
--                                then 1
--                                else undefined
--                    else if temp1 == 1
--                         then case temp2 of
--                                m:ms -> case temp3 of
--                                          _ -> m
--                         else if temp1 == 2
--                              then case temp2 of
--                                     _ -> let g = temp3
--                                          in case temp3 of
--                                               _ -> g
--                              else let m = temp
--                                   in 5
-- (\ s ->
-- case s of
--    (x,y) -> x + y) (5,6)

-- let temp = s
--  in let (temp1, temp2) = temp
--      in let x = temp1
--          in let y = temp2
--              in x + y


-- (\ s ->
-- case s of
--    (x,1) -> x
--    (1,y) -> y) (5,6)

-- let temp = s
--  in let (temp1, temp2) = temp
--      in if temp2 == 1
--         then let x = temp1
--               in x
--         else if temp1 == 1
--              then let y = temp2
--                   in  y

{-
constants lead to if
Var lead to let
ADT lead to case
wildcards,nil lead to let
tuple leads to a combination of the above

-}
