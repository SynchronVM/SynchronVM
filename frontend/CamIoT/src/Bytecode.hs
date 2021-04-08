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


import Debug.Trace

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
  | rewriteRequired p = translate (rewriteTuplesCase expr)
  | otherwise = C.Let (translatePat p) (translate cond) (translate exp)


{-

More than one pattern match expression where
first entry is a tuple. Possible rewrite to an if
or case or a combination

-}
translate expr@(SECase _ _ ((SPM (SPTup _ _ _) _):_)) =
  translate (rewriteTuplesCase expr)

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
      | rewriteRequired x ->
        case x of
          -- Just x like cases
          SPVar _ (AST.Ident var) -> ((tag, C.PatVar var), translate expr)
          SPWild _                -> ((tag, C.Empty), translate expr)
          SPNil  _                -> ((tag, C.Empty), translate expr)
          SPTup _ p1 p2           ->
            let computation = do
                  tempVar <- fresh
                  pure $ ((tag, C.PatPair (translatePat p1) (C.PatVar tempVar))
                         , translate
                           (SECase
                             (typeofpat p2)
                             (SEVar (typeofpat p2) (AST.Ident tempVar))
                             [(SPM p2 expr)]))
             in evalState (runCodegen computation) (initState 0)
          _ -> error "Const, ADT, @ patterns not handled at this level"

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
  let newLetExpr = SELet
                   ty1
                   (SPNAdt ty2 (AST.UIdent constr2) (Just pat))
                   (SEVar ty2 (AST.Ident tempVar))
                   ein
  expr' <- rewriteLet newLetExpr
  pure $
    C.Case
    (translate ebound)
    [((constr1, (C.PatPair (C.PatVar var1) (C.PatVar tempVar))), expr')
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

typeofpat (SPVar ty _)     = ty
typeofpat (SPConst ty _)   = ty
typeofpat (SPNAdt  ty _ _) = ty
typeofpat (SPWild  ty)     = ty
typeofpat (SPNil   ty)     = ty
typeofpat (SPTup ty _ _)   = ty
typeofpat (SPLay ty _ _)   = ty


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
path = "testcases/good7.cam"

test :: IO ()
test = do
  compiled <- compile path
  case compiled of
    Left err -> putStrLn err
    Right desugaredIr -> do
      putStrLn $ PP.printTree desugaredIr
      --putStrLn $ show desugaredIr
      let camir = translate desugaredIr
      -- let cam   = C.interpret camir
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

-- grault'' x = let (temp1, temp2, temp3) = x
--               in case temp1 of
--                    1 -> case temp2 of
--                           _ -> case temp3 of
--                                  3 -> 1
--                    1 -> case temp2 of
--                           m:ms -> case temp3 of
--                                     _ -> m
--                    2 -> case temp2 of
--                           _ -> case temp3 of
--                                  g@_ -> g
--                    _ -> case temp2 of
--                           _ -> case temp3 of
--                                  _ -> 5

-- grault'' x = let (temp1, temp2, temp3) = x
--               in case temp1 of
--                    1 -> case temp2 of
--                           _ -> case temp3 of
--                                  3 -> 1
--                    1 -> case temp2 of
--                           m:ms -> case temp3 of
--                                     _ -> m
--                    2 -> case temp2 of
--                           _ -> case temp3 of
--                                  g@_ -> g
--                    _ -> case temp2 of
--                           _ -> case temp3 of
--                                  _ -> 5


-- Step 1
-- Check if it is an ADT or a tuple
-- IF ADT apply ADT transform and then tupleTransform
-- Otherwise apply tupleTransform and then ADTTransform


-- Exhibit 1
-- case x of
--   1:2:3:Nil ->e
-- case x of
--   1:temp -> case temp of
--               2:temp2 -> case temp2 of
--                                3:Nil -> e
-- case x of
--   1:temp -> case temp of
--               2:temp2 -> case temp2 of
--                                3:Nil -> e



-- Exhibit 2
-- case x of
--   (1,2):(3,4): ms -> e


-- case x of
--   (1,2) : temp -> case temp of
--                    (3,4):ms -> e

-- case x of
--   t:temp -> let (t1,t2) = t
--              in case t1 of
--                   1 -> case t2 of
--                           2 -> case temp of
--                                  t3:ms -> let (t4,t5) = t3
--                                            in case t4 of
--                                                 3 -> case t5 of
--                                                        4 -> e

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

-- test1 a =
--   case a of
--     (b:bc, (d,e), _     ) -> 1
--     (f   , g    , Just h) -> 2

-- test1' a =
--   let (t1, (t2, t3), t4) = a
--    in let (t5, t6, t7)   = a
--       in case t1 of
--            b:bc -> case t2 of
--                   d -> case t3 of
--                          e -> case t4 of
--                                 temp -> 1
--            _    -> case t5 of
--                      f    -> case t6 of
--                                g -> case t7 of
--                                       Just h -> 2


-- case a of
--   ((x,y), z) ->
--   (m, n)     ->

-- let ((t1, t2), t3) = a
--  in let (t5, t6)   = a
--      in case t1 of
--           x -> case t2 of
--                  y -> case t3 of
--                         z -> e1
--           _ -> case t5 of
--                  m -> case t6 of
--                         n -> e2

-- let (t1, (t2, t3), t4) = a
--  in case t1 of
--       b:bc ->
--       f    ->

-- test2 x =
--   case x of
--     ((m:ms),(n:ns)):rf -> 1

--   case x of
--     t1:rf -> case t1 of
--                ((m:ms),(n:ns)) -> 1

--  case x of
--     t1:rf -> let (m1,m2) = t1
--               in case m1 of
--                    m:ms -> case m2 of
--                              n:ns -> 1


-- Considering this subset of patterns
-- data SPat a
--     = SPVar a AST.Ident
--     | SPNAdt a AST.UIdent (Maybe (SPat a))
--     | SPTup a (SPat a) (SPat a)
--   deriving (Eq, Ord, Show, Read)

{-
case (x,y) of
  (ST49 v64, v50)  -> e1
  (ST50 m, ST52 y) -> e2

-- REWRITTEN TO --

let (temp1, temp2) = (x, y) in
case temp1 of
   ST49 v64 -> case temp2 of
                    v50 -> e1
   ST50 m   -> case temp2 of
                    ST52 y -> e2
-}

rewriteTuplesCase :: SExp SType -> SExp SType
rewriteTuplesCase expr@(SECase _ _ (SPM (SPTup _ _ _) _ :_)) =
  evalState (runCodegen $ rewriteCasePair expr) (initState 0)
rewriteTuplesCase _ = error "Not a pair case"

rewriteCasePair :: SExp SType -> Codegen (SExp SType)
rewriteCasePair (SECase ty1 econd pm@(SPM tup@(SPTup ty2 p1 p2) e :_)) = do
  allTrees <- mapM (\(SPM p _) -> genTree p) pm
  let hs = map heightOfTree allTrees
  -- assume allEqual for now
  let finalTree = head allTrees
  let exprs = map (\(SPM _ e) -> e) pm
  let caseExpr = genCase ty1 (flattenTree finalTree) (map (\(SPM p _) -> flattenTree p) pm) exprs
  pure $ SELet ty1 finalTree econd caseExpr
  where
    genTree (SPNAdt aty _ _) = do
      tempVar <- fresh
      pure $ SPVar aty (AST.Ident tempVar)
    genTree (SPVar vty _) = do
      tempVar <- fresh
      pure $ SPVar vty (AST.Ident tempVar)
    genTree (SPTup tty p1 p2) = do
      p1' <- genTree p1
      p2' <- genTree p2
      pure $ SPTup tty p1' p2'
    genTree _ = error "Other patterns not covered"

    heightOfTree (SPTup _ p1 p2) =
      1 + max (heightOfTree p1) (heightOfTree p2)
    heightOfTree (SPVar _ _) = 1
    heightOfTree _ = error "Other patterns eliminated"

    flattenTree (SPTup _ p1 p2) = flattenTree p1 ++ flattenTree p2
    flattenTree x = [x]

    allEqual xs = length (nub xs) == 1
rewriteCasePair _ = error "Other case patterns not applied here"


{-
case (x,y) of
  (ST49 v64, v50) -> e1
  (ST50 m, ST52 y) -> e1

[t1,t2]
[[ST49 v64, v50]
,
 [ST50 m, ST52 y]]
let (t1, t2) = (x,y) in

case t1 of
  ST49 v64 ->
  ST40 m ->
-}

patToVar (SPVar vty ident) = SEVar vty ident
patToVar _ = error "Non var patterns not allowed"

genCase :: SType -> [SPat SType] -> [[SPat SType]] -> [SExp SType] -> (SExp SType)
genCase _ [] _ _ = error "Such cases do not occur"
genCase ty pats clauses finalEs =
  let (p, restPats, distinctClauses, restClauses) = chooseDistinctPatClause -- See Example 1 to understand why this was added
      repPats = replicate (length restClauses) restPats
      newCondClause = zipWith zip repPats restClauses
      basecases = map SPM distinctClauses
      allExprs = zipWith (gen ty) newCondClause finalEs
   in SECase ty (patToVar p) (zipWith (\f a -> f a) basecases allExprs)
   where
     chooseDistinctPatClause
       | length clauses == 1 =
           ( head pats
           , tail pats
           , map head clauses
           , map tail clauses)

       | otherwise = -- we know length of clauses is >1 so atleast 2
         let c1:c2:_ = clauses
             dcn      = distinctClauseNumber c1 c2 0
          in ( pats !! dcn
             , deleteN dcn pats
             , map (!! dcn) clauses
             , map (deleteN dcn) clauses
             )
          where
            distinctClauseNumber [] [] _ = 0
            distinctClauseNumber [] _ _  = 0 -- XXX: Assuming pairs are equally branched so this case doesn't arise
            distinctClauseNumber _ [] _  = 0 -- XXX: Assuming pairs are equally branched so this case doesn't arise
            distinctClauseNumber ((SPVar _ _):c1s) ((SPVar _ _):c2s) i =
              distinctClauseNumber c1s c2s (i + 1)
            distinctClauseNumber (_:c1s) (_:c2s) i = i


gen :: SType -> [(SPat SType, SPat SType)] -> SExp SType -> SExp SType
gen _ [] _ = error "Because of being a pair such case would never arise"
gen sty [(t, b)] finalExpr =
  SECase sty (patToVar t) [SPM b finalExpr]
gen sty ((t, b):xs) finalExpr =
  SECase sty (patToVar t) [SPM b (gen sty xs finalExpr)]



deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

{- EXAMPLE 1

argh x = case x of
           (t, Just m)  -> t
           (k, Nothing) -> k
argh' x =
  let (y,z) = x
  in case y of
       t -> case z of
              Just m -> t
       k -> case z of
              Nothing -> k

-- AFTER ADDING chooseDistinctPatClause
argh'' x =
 let (y,z) = x
  in case z of
       Just m -> case y of
                   t -> t
      Nothing -> case y of
                   k -> k

-}
