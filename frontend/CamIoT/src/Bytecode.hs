-- MIT License

-- Copyright (c) 2020,2021 Robert Krook, Abhiroop Sarkar and Joel Svensson

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
import LetLifting.LetLift
import Lib
import GHC.Word
import System.Exit

import qualified CamOpt as C
import qualified Assembler as A
import qualified Parser.AbsTinyCamiot as AST
import qualified Parser.PrintTinyCamiot as PP
import qualified Peephole as Peephole
import qualified Bytecode.InterpreterModel as IM



{- translate -
   SExp t is desugared IR from frontend and
   converts this to middleware Exp from CamOpt.hs.
   middleware Exp is "simpler"
-}
{- See NOTE 2 for missing rewrites -}
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
translate e@(SEAppF _ (AST.Ident ident) args) = C.Foreign $ C.ForeignCall ident (length args) (map translate args)
translate e@(SEApp _ e1 e2)
  | runtimeFuncs e = genrtsfunc e
  | otherwise      = C.App (translate e1) (translate e2)
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
    geneq STInt  = C.BEQ
    geneq STBool = C.BEQ
    geneq _      = error "EQ for other types not yet supported"

translate (SEAdd ty e1 aop e2) =
  let e1' = translate e1
      e2' = translate e2
   in C.Sys $ C.Sys2 addOp e1' e2'
   where
     addOp = case (ty, aop) of
               (STInt, AST.Plus _)    -> C.PlusI
               (STInt, AST.Minus _)   -> C.MinusI
               (STFloat, AST.Plus _)  -> C.PlusF
               (STFloat, AST.Minus _) -> C.MinusF
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

translate (SEMutR _ patexps e2) =
  C.Letrec (map (\(p,e) -> (translatePat p, translate e)) patexps) (translate e2)


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
translate (SECase _ cond [zzz@(SPM (SPVar _ (AST.Ident var)) exp)]) =
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


{-
case x of
  1 -> e1
  2 -> e2

-- REWRITE TO --

if x == 1
then  e1
else case x of
         2 -> e2
-- REWRITE TO --

if x == 1
then  1 ->
else if x == 2
     then e2
     else Nil
-}


translate expr@(SECase casety econd ((SPM (SPConst cty const) e):_)) =
  translate $ rewriteCaseConstants expr

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
          _ -> error "Const, ADT, @ patterns not handled at this level" -- ADTs wont occur. A pattern like x:y:ys encoded using Tuples which is handled already

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
-- Problematic cases--
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



rewriteLet e = error $ "Let not rewritten: " ++ show e




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

{- NOTE 2

Unhandled patterns:

1. The one given in NOTE 1 above
2. case x of
     (a, b) -> e1
     (c, (d, e)) -> e2
   Unevenly nested pairs. The nesting of the second
   clause is more than the nesting of the first.
3. case x of
    m:n:ys -> e1
    p:q:rs -> e2
   Case expressions with more than one clause which
   hase 2 or more constructors. Can be easily rewritten
   using a combination of rewrite-let and case


-}

condPutStrLn :: Bool -> String -> IO ()
condPutStrLn b s =
  case b of
    True -> putStrLn s
    False -> return ()

byteCompile :: Bool -> FilePath -> IO ([Word8], [String])
byteCompile verbose path = do
  compiled <- compile verbose path
  case compiled of
    Left err -> do putStrLn err
                   exitFailure 
    Right desugaredIr -> do

      condPutStrLn verbose $ "\nDesugared intermediate representation: \n"
      condPutStrLn verbose $ show desugaredIr--PP.printTree desugaredIr
      --putStrLn $ show desugaredIr

      condPutStrLn verbose $ "\nCAM IR (no pp): \n"
      let cam1 = translate desugaredIr
      condPutStrLn verbose $ show cam1

      condPutStrLn verbose $ "\nCAM BYTECODE (Hs Datatype): \n"
      let cam2   = C.interpret cam1
      condPutStrLn verbose $ show cam2

      condPutStrLn verbose $ "\nCAM BYTECODE (Peephole optimized): \n"
      let camopt   = Peephole.optimise $ cam2
      condPutStrLn verbose $ show camopt

      condPutStrLn verbose $ "\nCAM BYTECODE (uint8_t): \n"
      let (bytecode, foreign_arr) = A.translate camopt
      condPutStrLn verbose $ show bytecode

      --condPutStrLn verbose $ "\n\n CAM HS Interpreter \n\n"
      --let val = IM.evaluate $ C.interpret camir
      --condPutStrLn verbose $ show val

      -- condPutStrLn verbose $ "\nCAM Assembler and true bytecode generator: \n"
      -- A.genbytecode cam
      -- putStrLn $ show $ A.translate $ C.interpret $ translate desugaredIr
      return (bytecode, foreign_arr)


-- Experiments --
-- path = "testcases/mutrec_debug.cam"

path = "testcases/good28.cam"


test :: IO ()
test = do
  compiled <- compile True path
  case compiled of
    Left err -> putStrLn err
    Right desugaredIr -> do
      putStrLn $ PP.printTree desugaredIr
      putStrLn $ "\n\n Debug Follows \n\n"
      -- putStrLn $ show desugaredIr


      putStrLn $ "\n\n CAM IR (no pp) \n\n"
      let camir = translate desugaredIr
      putStrLn $ show camir


      putStrLn $ "\n\n CAM BYTECODE (Hs Datatype) \n\n"
      let cam   = Peephole.optimise $ C.interpret camir
      putStrLn $ show cam

      putStrLn $ "\n\n CAM BYTECODE (uint8_t) \n\n"
      let cam   = Peephole.optimise $ C.interpret camir
      putStrLn $ show $ A.translate cam

      putStrLn $ "\n\n CAM HS Interpreter \n\n"
      let val = IM.evaluate $ C.interpret camir
      -- putStrLn $ show val

      putStrLn $ "\n\n CAM Assembler and true bytecode generator \n\n"
      -- A.genbytecode cam
      -- putStrLn $ show $ A.translate $ C.interpret $ translate desugaredIr


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







-- We will translate a subset of patterns
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

-- See NOTE 3 to understand what is happening here
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

    -- Added a const case 
    genTree const@(SPConst _ _) = do
      pure const
    genTree (SPTup tty p1 p2) = do
      p1' <- genTree p1
      p2' <- genTree p2
      pure $ SPTup tty p1' p2'

    -- Replace wildcard with random variable.
    -- There are probably optimisations we could apply
    -- when generating code for wildcard, so that the unwanted
    -- data is immediately discarded.
    genTree (SPWild ty) = do
      tempVar <- fresh
      pure $ SPVar ty (AST.Ident tempVar)
    genTree c = error $ "Other patterns not covered: " ++ show c

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

-- NOTE 3 Step 4 onwards
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

patToVar (SPVar vty ident) = SEVar vty ident
patToVar e = error $ "Non var patterns not allowed: " ++ show e


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



{- NOTE 3

Given:
case (x,y) of
  (ST49 v64, v50)  -> e1
  (ST50 m, ST52 y) -> e2

Step 1:

map genTree on all clauses

[(temp1, temp2), (temp3, temp4)]

Assume all clauses are equally nested for now;
XXX: For supporting arbitrarily nested clauses
we need to do something here;

Step 2:

let (temp1, temp2) = (x,y)
 in ..rewrite_here..

Step 3:

Flatten all the trees:

Something like (a,(b,(c,d))) => [a,b,c,d] and the same for the clauses

Step 4 :

Step4 onwards happens in genCase:

Choose distinct Clause

Eg:

case x of
   (t, Just m)  -> t
   (k, Nothing) -> k

Here we need to choose the clause which will dictate the choice.
In this case it is the second column -> [Just m, Nothing]

Also it is sufficient to look at the first 2 clauses.
If all the entities are simply variables the 2nd clause (and onwards)
would never be hit. It will be always be limited to the first clause

Also if all the clauses are variables simply choose the first one.
Eg:

case x of
  (a,b,c) -> .... - Here a will be the distinct clause

See distinctClauseNumber

Step 5

After getting the distinct clause we do

let (temp1, temp2) = (x,y)
 in case temp1 of
        ST49 v64 -> case temp2 of
                         v50    -> e1
        ST50 m   -> case temp2 of
                         ST52 y -> e2
^ the original case

This one:
case x of
   (t, Just m)  -> t
   (k, Nothing) -> k

becomes

let (temp1, temp2) = x
 in case temp2 of
         Just m  -> case temp1 of
                          t -> t
         Nothing -> case temp2 of
                          k -> k


Step 6 : Other bookkeeping follows
-}


rewriteCaseConstants :: SExp SType -> SExp SType
rewriteCaseConstants expr@(SECase casety econd [(SPM (SPConst cty const) e)]) =
  SEIf casety condcheck e (SEConst casety AST.CNil)
  where
    condcheck = SERel STBool econd (AST.EQC (STLam (typeof econd) (STLam cty STBool))) (SEConst cty const)

rewriteCaseConstants expr@(SECase casety econd ((SPM (SPConst cty const) e):erest)) =
  SEIf casety condcheck e (rewriteCaseConstants (SECase casety econd erest))
  where
    condcheck = SERel STBool econd (AST.EQC (STLam (typeof econd) (STLam cty STBool))) (SEConst cty const)
rewriteCaseConstants e = e



runtimeFuncs :: SExp SType -> Bool
runtimeFuncs (SEApp _ (SEApp _ (SEApp _ (SEVar   _ (AST.Ident rtsfunc)) _) _) _)
  | rtsfunc == syncT = True
  | otherwise = False
runtimeFuncs (SEApp _ (SEApp _ (SEVar   _ (AST.Ident rtsfunc)) _) _)
  | rtsfunc == send   = True
  | rtsfunc == choose = True
  | rtsfunc == spawnExternal = True
  | rtsfunc == wrap   = True
  | otherwise       = False
runtimeFuncs (SEApp _ (SEVar   _ (AST.Ident rtsfunc)) _)
  | rtsfunc == sync = True
  | rtsfunc == channel = True
  | rtsfunc == recv    = True
  | rtsfunc == spawn   = True
  | otherwise          = False
runtimeFuncs _ = False


sync    = "sync"
send    = "send"
recv    = "recv"
spawn   = "spawn"
choose  = "choose"
channel = "channel"
spawnExternal = "spawnExternal"
wrap    = "wrap"
syncT   = "syncT"

genrtsfunc :: SExp SType -> C.Exp
genrtsfunc e@(SEApp _ (SEApp _ (SEApp _ (SEVar   _ (AST.Ident rtsfunc)) e2) e3) e4)
  | rtsfunc == syncT = C.Sys $ C.RTS3 C.SYNCT (translate e2) (translate e3) (translate e4)
  | otherwise = error $ "Incorrect expression type: " <> show e
genrtsfunc e@(SEApp _ (SEApp _ (SEVar   _ (AST.Ident rtsfunc)) e2) e3)
  | rtsfunc == send   = C.Sys $ C.RTS2 C.SEND   (translate e2) (translate e3)
  | rtsfunc == choose = C.Sys $ C.RTS2 C.CHOOSE (translate e2) (translate e3)
  | rtsfunc == spawnExternal =
    C.Sys $ C.RTS2 C.SPAWNEXTERNAL (translate e2) (translate e3)
  | rtsfunc == wrap = C.Sys $ C.RTS2 C.WRAP (translate e2) (translate e3)
  | otherwise = error $ "Incorrect expression type: " <> show e
genrtsfunc e@(SEApp _ (SEVar   _ (AST.Ident rtsfunc)) e2)
  | rtsfunc == sync    = C.Sys $ C.RTS1 C.SYNC    (translate e2)
  | rtsfunc == channel = C.Sys $ C.RTS1 C.CHANNEL (translate e2)
  | rtsfunc == recv    = C.Sys $ C.RTS1 C.RECV    (translate e2)
  | rtsfunc == spawn   = C.Sys $ C.RTS1 C.SPAWN   (translate e2)
  | otherwise = error $ "Incorrect expression type: " <> show e
genrtsfunc e = translate e
