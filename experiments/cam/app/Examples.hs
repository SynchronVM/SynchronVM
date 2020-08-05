-- MIT License

-- Copyright (c) 2020 Abhiroop Sarkar

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

module Examples where

import Bytecode.InterpreterModel
import CAM

example0 = Sys $ Sys2 Minus (Sys $ LInt 5) (Sys $ LInt 4)

example1 = Lam (PatVar "x") (Sys $ Sys2 Plus (Sys $ LInt 1) (Var "x"))

example2 = App example1 (Sys $ LInt 1)

example3 = Lam (PatVar "f") (Lam (PatVar "x") (App (Var "f") (App (Var "f") (Var "x"))))

example4 = Lam (PatVar "n") (If (Sys $ Sys2 BGE (Var "n") (Sys $ LInt 0)) (Var "n") (Sys $ Sys1 Neg (Var "n")))

{-
case s of
   Nil -> 5
   Cons _ _ -> 10
-}

example5 = Case (Var "s") [ (("Empty", Empty), (Sys $ LInt 5))
                          , (("::"   , Empty), (Sys $ LInt 10))
                          ]

{-
let a = 5 in
(a * a)
-}

example6 = Let (PatVar "a") (Sys $ LInt 5) (Sys $ Sys2 Multiply (Var "a") (Var "a"))

{-
letrec even = \n -> if (n == 0) then true else not (even (n - 1))
in even 56

generated

PUSH;
QUOTE (LInt 56);
SWAP;
REST 0;
CALL "label_1";
SKIP;
APP;
STOP;
"label_1" : CUR "label_2";
RETURN;
"label_2" : PUSH;
PUSH;
ACC 0;
SKIP;
SWAP;
QUOTE (LInt 0);
PRIM2 ==;
GOTOFALSE "label_3";
QUOTE (LBool True);
GOTO "label_4";
"label_3" : PUSH;
PUSH;
ACC 0;
SKIP;
SWAP;
QUOTE (LInt 1);
PRIM2 -;
SWAP;
REST 1;
CALL "label_1";
SKIP;
APP;
PRIM1 ~;
"label_4" : SKIP;
RETURN;
SKIP
-}
-- example7 = Letrec (PatVar "even") (Lam (PatVar "n") (If (Sys $ Sys2 BEQ (Var "n") (Sys $ LInt 0)) (Sys $ LBool True) (Sys $ Sys1 NOT (App (Var "even") (Sys $ Sys2 Minus (Var "n") (Sys $ LInt 1))))))
--                   (App (Var "even") (Sys $ LInt 56))

{-
letrec even = \n -> if (n == 0) then true else not (even (n - 1))
in even 56
-}

example7 = Letrec [( (PatVar "even")
                   , (Lam (PatVar "n") (If (Sys $ Sys2 BEQ (Var "n") (Sys $ LInt 0))
                                        (Sys $ LBool True)
                                        (Sys $ Sys1 NOT (App (Var "even") (Sys $ Sys2 Minus (Var "n") (Sys $ LInt 1)))))))
                  ]
                  (App (Var "even") (Sys $ LInt 56))

{-
let y = 1 in
(\x -> x + y)
-}

example8 = Let (PatVar "y") (Sys $ LInt 1)
           (Lam (PatVar "x") (Sys $ Sys2 Plus (Var "x") (Var "y")))

{-
let y = 1 in
(\x -> x + y) 4
-}

example9 = Let (PatVar "y") (Sys $ LInt 1)
               (App (Lam (PatVar "x") (Sys $ Sys2 Plus (Var "x") (Var "y"))) (Sys $ LInt 4))

{-
(let y = 1 in
 (\x -> x + y)) 4
-}

example10 = App (Let (PatVar "y") (Sys $ LInt 1)
                 (Lam (PatVar "x") (Sys $ Sys2 Plus (Var "x") (Var "y")))) (Sys $ LInt 4)


{-
let foo = let m = \x -> x
           in 3
  in let baz = foo + 2
      in (baz + 4)
-}

example11 = Let (PatVar "foo") (Let (PatVar "m") (Lam (PatVar "x") (Var "x")) (Sys $ LInt 3))
                (Let (PatVar "baz") (Sys $ Sys2 Plus (Var "foo") (Sys $ LInt 2))
                     (Sys $ Sys2 Plus (Var "baz") (Sys $ LInt 4))
                )

{-
let foo = let m = 11
           in 3
  in let baz = foo + 2
      in (baz + 4)
-}

example12 = Let (PatVar "foo") (Let (PatVar "m") (Sys $ LInt 11) (Sys $ LInt 3))
                (Let (PatVar "baz") (Sys $ Sys2 Plus (Var "foo") (Sys $ LInt 2))
                     (Sys $ Sys2 Plus (Var "baz") (Sys $ LInt 4))
                )

{-
-- Closure should be heap allocated
but stackroot points to the closure throughout
let foo = let m = 11
           in \x -> x
  in let baz = foo 2
      in (baz + 4)
-}

example13 = Let (PatVar "foo") (Let (PatVar "m") (Sys $ LInt 11) (Lam (PatVar "x") (Var "x")))
                (Let (PatVar "baz") (App (Var "foo") (Sys $ LInt 2))
                     (Sys $ Sys2 Plus (Var "baz") (Sys $ LInt 4))
                )
