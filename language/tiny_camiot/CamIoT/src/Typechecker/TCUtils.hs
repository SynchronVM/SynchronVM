-- MIT License

-- Copyright (c) 2020 Robert Krook

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
module Typechecker.TCUtils
       (
         TCError(..)

         -- * Functions that inspects the AST
       , isMoreGeneral
       , usesVar
       ) where

import Parser.AbsTinyCamiot
    ( Const,
      Exp(..),
      Ident(..),
      Pat(..),
      PatMatch(..),
      Type(TTup, TLam, TVar, TAdt),
      UIdent(..) )
import Parser.PrintTinyCamiot ( printTree )

import Data.List ( intercalate )

-- All potential errors that can be thrown. Feel free to
-- add more variants as you need.
-- They don't include much information about the source now, so they can be tricky to read.
data TCError =
    InfiniteType Ident Type
  | UnificationFail Type Type
  | TypeError (Exp ()) Type Type
  | PatternTypeError (Pat ()) Type Type
  | UnboundVariable String
  | UnboundConstructor UIdent
  | DuplicateTypeSig Ident
  | DuplicateConstructor UIdent Type
  | TypeArityError UIdent [Type] [Type]
  | WrongConstructorGoal UIdent Type Type
  | LambdaConstError Const
  | CaseExpressionError (Pat ()) Type Type
  | ConstructorNotFullyApplied UIdent Int Int
  | UnboundTypeVariable [Ident] [Ident]
  | TypeSignatureTooGeneral Ident Type Type
  | FunctionClausesNotEqual Ident Type Type
  | FunctionClauseWrongType Ident Type Type
  | RecursiveFunctionWithoutTypesig Ident
  | AloneTypeSignature Ident Type

instance Show TCError where
    show (InfiniteType var t) =
        "Type error ---\n" ++
        "Failed to create the infinite type from type variable " ++ printTree var ++ 
        " with type " ++ printTree t
    show (UnificationFail t1 t2) =
        "Type error ---\n" ++
        "Failed to unify the two types: \n" ++
        printTree t1 ++ " and \n" ++
        printTree t2
    show (TypeError exp t1 t2) =
        "Type error ---\n" ++
        "Could not match the expected type of " ++ printTree exp ++ " with the actual type.\n" ++
        "Expected: " ++ printTree t1 ++ "\n" ++
        "Actual:   " ++ printTree t2
    show (PatternTypeError pat t1 t2) =
        "Type error ---\n" ++
        "Cannot match the type of pattern " ++ printTree pat ++ " with the expected type\n" ++
        "Actual type:   " ++ printTree t1 ++ "\n" ++
        "Expected type: " ++ printTree t2
    show (UnboundVariable var) =
        "Type error ---\n" ++
        "Unbound variable: " ++ var
    show (UnboundConstructor (UIdent con)) =
        "Type error ---\n" ++
        "Unbound constructor: " ++ show con
    show (DuplicateTypeSig (Ident fun)) =
        "Type error ---\n" ++
        "Type signature for " ++ fun ++ " declared more than once"
    show (DuplicateConstructor c t) =
        "Type error ---\n" ++
        "Data constructor " ++ show c ++ " : " ++ printTree t ++ " declared more than once"
    show (TypeArityError con' tvars vars) =
        "Type error ---\n" ++
        "Arity error - declared type " ++ printTree (TAdt con' tvars) ++ " does not match " ++
        "inferred type " ++ printTree (TAdt con' vars)
    show (WrongConstructorGoal con inferred declared) =
        "Type error ---\n" ++
        "Constructor " ++ printTree con ++ " attempts to create a value of type " ++ 
        printTree inferred ++ ", but it has been declared to be of form " ++ printTree declared
    show (LambdaConstError c) =
        "Type error ---\n" ++
        "Lambdas can only abstract over variables, not constants such as " ++ printTree c
    show (CaseExpressionError pat inferred expected) =
        "Type error ---\n" ++
        "Pattern " ++ printTree pat ++ " is of the wrong type \n" ++
        "Expected: " ++ printTree expected ++ "\n" ++
        "Inferred: " ++ printTree inferred
    show (ConstructorNotFullyApplied con expected found) =
        "Type error ---\n" ++
        "Data constructor " ++ printTree con ++ " applied to " ++ show found ++ " arguments, " ++
        "but " ++ show expected ++ " is expected"
    show (UnboundTypeVariable bound free) =
        "Type error ---\n" ++
        "Encountered unbound type variable \n" ++
        "Bound: " ++ intercalate "," (map printTree bound) ++ "\n" ++
        "Encountered: " ++ intercalate "," (map printTree free)
    show (TypeSignatureTooGeneral fun t1 t2) =
        "Type error ---\n" ++
        "The declared type of " ++ printTree fun ++ " is more general than the inferred one:\n" ++
        "Declared type: " ++ printTree t1 ++ "\n" ++
        "Inferred type: " ++ printTree t2
    show (FunctionClausesNotEqual id t1 t2) =
        "Type error ---\n" ++
        "The inferred types of the two function bodies are not the same\n" ++
        printTree t1 ++ " and\n" ++
        printTree t2
    show (FunctionClauseWrongType name t1 t2) =
        "Type error ---\n" ++
        "Definition of clause for function " ++ printTree name ++ " does not match inferred type\n" ++
        "Declared type: " ++ printTree t1 ++ "\n" ++
        "Actual type:   " ++ printTree t2
    show (RecursiveFunctionWithoutTypesig name) =
        "Type error ---\n" ++
        "Recursive functions must have an accompanying type signature, declared above the first function clause\n" ++
        printTree name ++ " does not have such a type signature"
    show (AloneTypeSignature name t) =
        "Type error ---\n" ++
        "The type signature " ++ printTree name ++ " : " ++ printTree t ++ " lacks " ++
        "an accompanying function definition"

{- | Checks if the first type is more general than the second type. Returns `Nothing`
if the shapes are different, Just False if the shapes are the same and they are equally
general, and Just True if the first type is more general than the second.
It boils down to check if in every place in the first type where there is a type variable
there is also a type variable in the second type.
-}
isMoreGeneral :: Type -> Type -> Maybe Bool
isMoreGeneral (TLam t1 t1s) (TLam t2 t2s) = (||) 
    <$> isMoreGeneral t1 t2
    <*> isMoreGeneral t1s t2s
isMoreGeneral (TVar _) (TVar _)               = Just False
isMoreGeneral (TVar _) _                      = Just True
isMoreGeneral (TAdt con1 t1s) (TAdt con2 t2s) =
    let maybes = zipWith isMoreGeneral t1s t2s
    in foldl (\b1 b2 -> (||) <$> b1 <*> b2) (Just False) maybes
isMoreGeneral (TTup t1s) (TTup t2s) = 
    let maybes = zipWith isMoreGeneral t1s t2s
    in foldl (\b1 b2 -> (||) <$> b1 <*> b2) (Just False) maybes
isMoreGeneral _ _ = Nothing

{-- | Checks if the argument identifier occurs in the argument expression.
Used to check for e.g if a function is recursive.
-}
usesVar
    :: Ident  -- ^ Argument identifier, e.g fac
    -> Exp a  -- ^ Argument expression, e.g n * fac (n-1)
    -> Bool
usesVar id e = case e of
    (ETup a texps)    -> any (usesVar id) texps
    (ECase a e1 br)   -> usesVar id e1 || any (usesVarPatMatch id) br
    (ELet a p e1 e2)  -> usesVarPat id p || usesVar id e1 || usesVar id e2
    (ELetR a p e1 e2) -> usesVarPat id p || usesVar id e1 || usesVar id e2
    (ELam a p e1)     -> usesVarPat id p || usesVar id e1
    (EIf a e1 e2 e3)  -> usesVar id e1 || usesVar id e2 || usesVar id e3
    (EApp a e1 e2)    -> usesVar id e1 || usesVar id e2
    (EOr a e1 e2)     -> usesVar id e1 || usesVar id e2
    (EAnd a e1 e2)    -> usesVar id e1 || usesVar id e2
    (ERel a e1 op e2) -> usesVar id e1 || usesVar id e2
    (EAdd a e1 op e2) -> usesVar id e1 || usesVar id e2
    (EMul a e1 op e2) -> usesVar id e1 || usesVar id e2
    (ENot a e1)       -> usesVar id e1
    (EVar a id')      -> id == id'
    (EUVar a _)       -> False
    (EConst a c)      -> False

-- | Checks if the argument identifier appears in the argument pattern.
usesVarPat
    :: Ident  -- ^ Argument identifier, e.g x
    -> Pat a  -- ^ Argument pattern, e.g (x,3)
    -> Bool
usesVarPat id p = case p of
    PConst _ c       -> False
    PVar _ id'       -> id == id'
    PZAdt _ _        -> False
    PNAdt _ _ pats   -> any (usesVarPat id) pats
    PWild _          -> False
    PNil _           -> False
    PTup _ patterns  -> any (usesVarPat id) patterns
    PLay _ id' pat   -> id == id' || usesVarPat id pat

{-- | Checks if the argument identifier appears in the pattern match clause,
which is just a pair of a pattern and an expression.
-}
usesVarPatMatch :: Ident -> PatMatch a -> Bool
usesVarPatMatch id (PM pat exp) = usesVarPat id pat || usesVar id exp