module CAM where

type Var = String

data Exp = Var Var  -- variable
         | Sys Sys  -- Primops
         | Void     -- Empty Tuple
         | Pair Exp Exp    -- Pair
         | Con  Exp        -- Constructed Value
         | App Exp Exp     -- Function application
         | Lam Pat Exp     -- Lambda Abstraction
         | If Exp Exp Exp  -- if then else
         | Let Pat Exp Exp -- Let bindings
         | Letrec Pat Exp Exp -- letrec
         | Case Exp [(Exp, Exp)]
         --            ^    ^
         --     constructor |
         --              resulting expression
         deriving (Ord, Show, Eq)

data Sys = Sys2 Exp Exp -- BinOp
         | Sys1 Exp     -- UnaryOp
         | LInt Int     -- Int s(0) in cam
         | LBool Bool   -- Bool s(0) in cam
         deriving (Ord, Show, Eq)

data Pat = PatVar Var
         | Empty
         | PatPair Pat Pat
         | As Var Pat      -- var `as` pat; equivalent to @ in Haskell
         deriving (Ord, Show, Eq)

data Instructions
   = ACC   Int  -- access the nth component of the environment register
   | QUOTE Sys  -- load immediate i.e `li` from the code area to environment
   | PUSH       -- copy content of register to stack
   | SWAP       -- interchange between register and topmost element of stack
   | CUR   Exp  -- build a closure with Exp and the environment register
   | RETURN     -- return from a subroutine call

-- NOTE:
{-
1. Use the stack for intermediate storage of environment;
   Always PUSH before beginning computation
2. BinOp (s(2)) expects first argument on stack and second on register

-}
