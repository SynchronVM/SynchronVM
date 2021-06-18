-- MIT License

-- Copyright (c) 2021 Abhiroop Sarkar

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

module MemoryExperiment.Examples where

import MemoryExperiment.CamOptMem
import MemoryExperiment.PeepholeMem
import MemoryExperiment.StaticMemoryManagedInterpreter
-- Examples
{-

g p = let e1 = (5,6) in
      p

main = let m = (3,2) in
       let a = (g m) in
       a
-- Rewritten to --

let v0 = \ v5 -> case v5 of {
  v1 -> let v2 = (5, 6) in v1
}
in let v3 = (3, 2) in let v4 = v0 v3 in v4

-- Rewritten to --

let v0 = \ v5 ->
           let v1 = v5 in
           let v2 = (5, 6) in
           v1 in
 let v3 = (3, 2) in
 let v4 = v0 v3  in
 v4


-}
example0 =
  Let (PatVar "v0") (Lam (PatVar "v5")
                     (Let (PatVar "v1") (Var "v5")
                      (Let (PatVar "v2") (Pair (Sys (LInt 5)) (Sys (LInt 6)))
                       (Var "v1"))))
  (Let (PatVar "v3") (Pair (Sys (LInt 3)) (Sys (LInt 2)))
   (Let (PatVar "v4") (App (Var "v0") (Var "v3"))
    (Var "v4")))


{-

g p = let e1 = (5,6) in
      let x = p in
      e1

main = let m = (3,2) in
       let a = (g m) in
       a

-- REWRITE --

let v0 = \ v6 -> case v6 of {
  v1 -> let v2 = (5, 6) in let v3 = v1 in v2
}
in let v4 = (3, 2) in let v5 = v0 v4 in v5

-- REWRITE --

let v0 = \ v6 ->
           let v1 = v6 in
           let v2 = (5,6) in
           let v3 = v1 in
           v2 in
 let v4 = (3,2) in
 let v5 = (v0, v4) in
 v5
-}


example1 =
  Let (PatVar "v0") (Lam (PatVar "v6")
                     (Let (PatVar "v1") (Var "v6")
                      (Let (PatVar "v2") (Pair (Sys (LInt 5)) (Sys (LInt 6)))
                       (Let (PatVar "v3") (Var "v1")
                        (Var "v2")))))
  (Let (PatVar "v4") (Pair (Sys (LInt 3)) (Sys (LInt 2)))
   (Let (PatVar "v5") (App (Var "v0") (Var "v4"))
    (Var "v5")))

-- CAM bytecode for palloc
--example1cam = Seq (Ins (COMB (Label 1))) (Seq (Ins MOVE) (Seq (Ins (QUOTE (LInt 3))) (Seq (Ins MOVE) (Seq (Ins (QUOTE (LInt 2))) (Seq (Ins CONS) (Seq (Ins CONS) (Seq (Ins PUSH) (Seq (Ins SND) (Seq (Ins SWAP) (Seq (Ins FST) (Seq (Ins APP) (Seq (Ins STOP) (Seq (Lab (Label 1) (Ins (REST 0))) (Seq (Ins MOVE) (Seq (Ins (QUOTE (LInt 5))) (Seq (Ins MOVE) (Seq (Ins (QUOTE (LInt 6))) (Seq (Ins CONSP) (Seq (Ins CONS) (Seq (Ins PUSH) (Seq (Ins FST) (Seq (Ins CONS) (Seq (Ins (ACC 1)) (Seq (Ins RETURN) (Lab (Label 65535) (Ins STOP))))))))))))))))))))))))))



{-
foo x = let m = \y -> y + 3
         in m x

main = foo 5

-- REWRITTEN --

let v5 = \ v6 -> case v6 of {
  v4 -> v4 + 3
}
in let v0 = \ v7 -> case v7 of {
  v1 -> let v2 = v5 in v2 v1
}
in v0 5

-- REWRITTEN --

let v5 = \ v6 -> let v4 = v6 in
                 v4 + 3 in
let v0 = \ v7 -> let v1 = v7 in
                 let v2 = v5 in
                 v2 v1 in
v0 5
-}

example2 =
  Let (PatVar "v5") (Lam (PatVar "v6")
                     (Let (PatVar "v4") (Var "v6")
                      (Sys (Sys2 PlusI (Var "v4") (Sys (LInt 3))))))
  (Let (PatVar "v0") (Lam (PatVar "v7")
                      (Let (PatVar "v1") (Var "v7")
                       (Let (PatVar "v2") (Var "v5")
                        (App (Var "v2") (Var "v1")))))
    (App (Var "v0") (Sys (LInt 5))))




run = evaluate . optimise . interpret



{-

foo = let x = 5 in
      \y -> y + x

main = foo 3

-- REWRITTEN --

let v5 = \ v6 -> \ v7 -> case (v6, v7) of {
  (v4, v3) -> v3 + v4
}
in let v0 = let v1 = 5 in v5 v1 in v0 3

-- REWRITTEN --

let v5 = \ v6 ->
              \ v7 ->
                   let (v4, v3) = (v6, v7) in
                   v3 + v4 in
let v0 = let v1 = 5
          in v5 v1 in
v0 3

-}


example3 =
  Let (PatVar "v5")
  (Lam (PatVar "v6")
   (Lam (PatVar "v7")
    (Let (PatPair (PatVar "v4") (PatVar "v3")) (Pair (Var "v6") (Var "v7"))
     (Sys (Sys2 PlusI (Var "v3") (Var "v4"))))))
  (Let (PatVar "v0") (Let (PatVar "v1") (Sys (LInt 5))
                      (App (Var "v5") (Var "v1")))
    (App (Var "v0") (Sys (LInt 3))))

--example3cam = Seq (Ins (COMB (Label 1))) (Seq (Ins MOVE) (Seq (Ins (QUOTE (LInt 5))) (Seq (Ins CONS) (Seq (Ins PUSH) (Seq (Ins SND) (Seq (Ins SWAP) (Seq (Ins FST) (Seq (Ins APP) (Seq (Ins MOVE) (Seq (Ins (QUOTE (LInt 3))) (Seq (Ins SWAP) (Seq (Ins APP) (Seq (Ins STOP) (Seq (Lab (Label 2) (Ins PUSH)) (Seq (Ins FST) (Seq (Ins SWAP) (Seq (Ins SND) (Seq (Ins CONS) (Seq (Ins PUSH) (Seq (Ins SND) (Seq (Ins SWAP) (Seq (Ins FST) (Seq (Ins (PRIM2 PlusI)) (Seq (Ins RETURN) (Seq (Lab (Label 1) (Ins (CURP (Label 2)))) (Seq (Ins RETURN) (Lab (Label 65535) (Ins STOP))))))))))))))))))))))))))))
