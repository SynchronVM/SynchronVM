{-| This module contains the core syntax representation of programs. The parser will e.g
produce a program in the form of the AST in this module. -}
module CamIoT.Internal.Syntax where

import Data.List  ( find
                  , delete
                  , groupBy
                  , partition
                  )
import Data.Maybe ( fromJust )

-- | Identifiers in the source language
data Ident
    = Ident String
    deriving (Eq, Show, Ord)

-- | Uppercase identifiers. Used for algebraic data types and constructors.
data UIdent
    = UIdent String
    deriving (Eq, Show, Ord)

-- | The types that are supported by the surface language
data Type
    = TVar Ident          -- ^ Type variables
    | TInt                -- ^ Integers
    | TFloat              -- ^ Floating point numbers
    | TBool               -- ^ Booleans
    | TNil                -- ^ Unit type
    | TTup [Type]         -- ^ Arbitrarily sizes tuples
    | TAdt UIdent [Type]  -- ^ Algebraic data types
    | TLam Type Type      -- ^ Function types
    deriving (Eq, Show)

{- | Get the arguments of a function type. E.g

@
> args (Int -> Bool -> Maybe a -> Float)
[Int, Bool, Maybe a]
@

-}
args :: Type -> [Type]
args (TLam t1 t2) = t1 : args t2
args _            = []

{- | Returns what a function type constructs. E.g

@
> construction (Int -> Bool -> Maybe a -> Float)
Float
@

and

@
> construction Float
Float
@

-}
construction :: Type -> Type
construction (TLam t1 t2) = construction t2
construction t            = t

{- | Create a function type of a list of types. E.g

@
> funtype [Int, Bool, Maybe a]
Int -> Bool -> Maybe a
@

-}
funtype :: [Type] -> Type
funtype [t]        = t
funtype (t1:t2:ts) = TLam t1 (funtype (t2:ts))

-- | Patterns supported by the surface language
data Pat a
    = PVar a Ident           -- ^ @x@
    | PNil a                 -- ^ Unit constant @()@
    | PConst a Lit           -- ^ Constants, e.g @1@, @True@
    | PWild a                -- ^ @_@
    | PAs a Ident (Pat a)    -- ^ @x as (y,z)@
    | PAdt a UIdent [Pat a]  -- ^ @Maybe a@, @Either a (Maybe b)@
    | PTup a [Pat a]         -- ^ @(a,b)@, @(a,b,c)@, @(a,(b,c))@
    deriving (Eq, Show)

-- | Get the value of the type variable @a@ out of a @Pat a@.
patVar :: Pat a -> a
patVar p = case p of
  PVar a _   -> a
  PNil a     -> a
  PConst a _ -> a
  PWild a    -> a
  PAs a _ _  -> a
  PAdt a _ _ -> a
  PTup a _   -> a

-- | Literals in the surface language
data Lit
    = LInt Int       -- ^ @1@, @24354@, @-342354@
    | LFloat Double  -- ^ @0.232@, @34234.2321@
    | LBool Bool     -- ^ @True@, @False@
    | LNil           -- ^ @()@
    deriving (Eq, Show)

-- | Expressions in the surface language
data Exp a
    = EVar a Ident                      -- ^ @x@
    | ECon a UIdent                     -- ^ @Just@, @Left@, ...
    | ELit a Lit                        -- ^ @1@, @True@, @()@
    {- |
    @
    case e of
        (a,0) -> Nothing
        (a,b) -> Just 5
    @
    -}
    | ECase a (Exp a) [(Pat a, Exp a)]
    | ETup a [(Exp a)]                  -- ^ @(1,2)@, @(1,(2,3))@, ...
    | EBin a (Exp a) (Exp a) (Binop a)  -- ^ @1 + 2@, @True && False@, ...
    | EUn a (Exp a) (Unop a)            -- ^ @! True@, ...
    | ELam a (Pat a) (Exp a)            -- ^ @\\x -> body@
    | EApp a (Exp a) (Exp a)            -- ^ @f x@
    | ELet a (Pat a) (Exp a) (Exp a)    -- ^ @let x = 5 in x + 5@
    | EIf a (Exp a) (Exp a) (Exp a)     -- ^ @if True then false else True@
    deriving (Eq, Show)

-- | Get the value of the type variable @a@ out of an @Exp a@.
expVar :: Exp a -> a
expVar e = case e of
  EVar a _     -> a
  ECon a _     -> a
  ELit a _     -> a
  ECase a _ _  -> a
  ETup a _     -> a
  EBin a _ _ _ -> a
  EUn a _ _    -> a
  ELam a _ _   -> a
  EApp a _ _   -> a
  ELet a _ _ _ -> a
  EIf a _ _ _  -> a

-- | Set the value of the type variable @a@ of a @Binop a@.
data Binop a
    = Add a  -- ^ @+@,  either @Float -> Float -> Float@ or @Int -> Int -> Int@
    | Sub a  -- ^ @-@,  either @Float -> Float -> Float@ or @Int -> Int -> Int@
    | Mul a  -- ^ @*@,  either @Float -> Float -> Float@ or @Int -> Int -> Int@
    | Div a  -- ^ @/@,  either @Float -> Float -> Float@ or @Int -> Int -> Int@
    | OLT a  -- ^ @<@,  either @Float -> Float -> Bool@ or @Int -> Int -> Bool@
    | OLE a  -- ^ @<=@, either @Float -> Float -> Bool@ or @Int -> Int -> Bool@
    | OGT a  -- ^ @>@,  either @Float -> Float -> Bool@ or @Int -> Int -> Bool@
    | OGE a  -- ^ @>=@, either @Float -> Float -> Bool@ or @Int -> Int -> Bool@
    {- | @==@, @a -> a -> Bool@, might need to change this since it is not obvious
    how to compare all types. -}
    | OEQ a
    | And a  -- ^ @&&@, @Bool -> Bool -> Bool@
    | Or a  -- ^ @||@, @Bool -> Bool -> Bool@
    deriving (Eq, Ord, Show)

-- | Get the value of the type variable @a@ out of a @Binop a@.
binopVar :: Binop a -> a
binopVar op = case op of
  Add a -> a
  Sub a -> a
  Mul a -> a
  Div a -> a
  OLT a -> a
  OLE a -> a
  OGT a -> a
  OGE a -> a
  OEQ a -> a
  And a -> a
  Or  a -> a

-- | Set the value of the type variable @a@ of a @Binop a@.
setBinopVar :: a -> Binop b -> Binop a
setBinopVar a op = case op of
  Add _ -> Add a
  Sub _ -> Sub a
  Mul _ -> Mul a
  Div _ -> Div a
  OLT _ -> OLT a
  OLE _ -> OLE a
  OGT _ -> OGT a
  OGE _ -> OGE a
  OEQ _ -> OEQ a
  And _ -> And a
  Or _  -> Or a

-- | Unary operators in the surface language
data Unop a
    = Not a  -- ^ Boolean negation, @!@
    deriving (Eq, Show, Ord)

-- | Get the value of the type variable @a@ out of a @Unop a@.
unopVar :: Unop a -> a
unopVar op = case op of
  Not a -> a

-- | Set the value of the type variable @a@ of a @Unop a@.
setUnopVar :: a -> Unop b -> Unop a
setUnopVar a op = case op of
  Not _ -> Not a

{- | Top level definitions in the language. A top level definition is either a
type signature, a function equation or a data type declaration.

@
> f : Int -> Int
DTypeSig (Ident "f") (TLam TInt TInt)
@

@
> f x = x
DEquation (TLam (TVar "a") (TVar "a")) (Ident "f") [PVar "x"] (EVar (Ident "x")))
@

@
> f x = x
> f 5 = 3
[ DEquation (TLam (TVar "a") (TVar "a")) (Ident "f") [PVar "x"] (EVar (Ident "x")))
, DEquation TInt (Ident "f") [PConst (LInt 5)] (ELit (LInt 3)))
]
@

@
> data Test a where
>   TestConstructor : Test a
DDataDec
  (UIdent "Test")
  [Ident "a"]
  [(UIdent "TestConstructor", TAdt (UIdent "Test") [Ident "a"])]
@

-}
data Def a
    = DTypeSig Ident Type                       -- ^ Type signatures
    | DEquation a Ident [Pat a] (Exp a)         -- ^ Function definitions
    | DDataDec UIdent [Ident] [(UIdent, Type)]  -- ^ Data type declarations
    deriving (Eq, Show)

{- | Get the name of a top level definition. Partial function which has no equation
for the case when it is applied to a data type declaration. -}
getName :: Def a -> Ident
getName (DTypeSig id _)      = id
getName (DEquation _ id _ _) = id

-- | Get the value of the type variable @a@ out of a @Def a@.
defVar :: Def a -> Maybe a
defVar d = case d of
  DTypeSig _ _      -> Nothing
  DEquation a _ _ _ -> Just a

{-| Functions in the surface language. After parsing, a program is represented as a list
of @Def ()@. Just by looking at this list it is not clear which definitions belong to
which functions. E.g the program

@
f 5 = 5
f 3 = 4
g x = x
@

is parsed as

@
[ DEquation TInt (Ident "f") [PConst (LInt 5)] (ELit (LInt 5)))
, DEquation TInt (Ident "f") [PConst (LInt 3)] (ELit (LInt 4)))
, DEquation (TLam (TVar "a") (TVar "a")) (Ident "g") [PVar "x"] (EVar (Ident "x")))
]
@

A better representation is to group functions together, like this:

@
[ [ DEquation TInt (Ident "f") [PConst (LInt 5)] (ELit (LInt 5)))
  , DEquation TInt (Ident "f") [PConst (LInt 3)] (ELit (LInt 4)))
  ]
, [ DEquation (TLam (TVar "a") (TVar "a")) (Ident "g") [PVar "x"] (EVar (Ident "x"))) ]
]
@

It is essentially this that a `Function` represents. The `Function` data type also houses
information about the type signature (if any exists) and the name of the function. After
type checking, the @typesig@ field should always be @Just x@ where x is the type of the
function.
-}
data Function a = Function
  { name      :: Ident       -- ^ Name of the function
  , equations :: [([Pat a], Exp a)]  -- ^ Equations that makes up the function definition
  , typesig   :: Maybe Type  -- ^ Type signature of the function, if any exists
  }
  deriving (Eq)

{- | Type synonym for Algebraic Datatypes (ADT). An ADT has a type constructor, zero or
more type parameters and one or more data constructors. -}
type ADT = (UIdent, [Ident], [(UIdent, Type)])

{- | A program in the surface language consists of three things. Zero or more datatype
declarations, zero or more functions and exactly 1 'main' function. -}
data Program a = Program
  { datatypes :: [ADT]         -- ^ Declared datatypes
  , functions :: [Function a]  -- ^ Functions
  , main      :: Function a    -- ^ Main
  }

{- | Take a @[Def ()]@ from the parser and turn it into a @Program ()@. Please read
the documentation for `Function` to see what this step is doing. If it's not possible to
do this conversion, it will return @Nothing@. The only case where it is impossible to do
this transformation is when there is no main-function present. -}
mkProgram :: Eq a => [Def a] -> Maybe (Program a)
mkProgram defs =
  let (datadecs,funs)    = partitionDataDecs defs
      functions          = mkFunctions funs
  in case partitionMain functions of
      Just (main, functions') -> Just $ Program (map unwrapDataDec datadecs) functions' main
      Nothing -> Nothing
  where
      unwrapDataDec (DDataDec uid vars cons) = (uid, vars, cons)

{- | Partition a @[Def a]@ into two lists where the first list contains all the
datatype declarations and the other contains all the functions. -}
partitionDataDecs :: [Def a] -> ([Def a], [Def a])
partitionDataDecs ds = partition pred ds
  where
      pred d = case d of
        DDataDec _ _ _ -> True
        _              -> False

{- | Take a @[Def a]@ and turn it into a @[Function a]@, which is explained in a bit
more detail in the documentation for `Function`. The @[Def a]@ represents the equations
and type signature (if any) that make up a single function. -}
mkFunctions :: [Def a] -> [Function a]
mkFunctions defs = map toFunction $ groups defs

{- | Take a list of the functions that makes up a program and separate out the main
function from them. If there is no main-function present, returns @Nothing@. Otherwise,
returns a pair @(main-function, rest-of-the-functions)@. -}
partitionMain :: Eq a => [Function a] -> Maybe (Function a, [Function a])
partitionMain funs = let main  = find ((==) (Ident "main") . name) funs
                     in case main of
                         Just m  -> Just (m, delete m funs)
                         Nothing -> Nothing

{- | Predicate to check if two named top level definitions are the same. Will
crash if the top level definition is a datatype declaration. -}
pred :: Def a -> Def a -> Bool
pred d1 d2 = getName d1 == getName d2

{- | Take a @[Def a]@ and turn it into a @[[Def a]]@, where each inner @[Def a]@ contains
the equations and potential type signature that make up a single function. -}
groups :: [Def a] -> [[Def a]]
groups defs = groupBy CamIoT.Internal.Syntax.pred defs

{- | Takes a list of definitions that make up a single function and turns it into a
`Function` object. If the list of definitions begins with a type signature, it is assumed
that the following definitions are not empty, that there are no signatures without
function definitions. -}
toFunction :: [Def a] -> Function a
toFunction (d:ds) = case d of
  DTypeSig id t      -> Function id (map defToPair ds) (Just t)
  DEquation _ id _ _ -> Function id (map defToPair (d:ds)) Nothing
  where
     defToPair :: Def a -> ([Pat a], Exp a)
     defToPair (DEquation _ _ args body) = (args, body)
