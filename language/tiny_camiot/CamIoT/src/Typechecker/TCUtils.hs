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
module Typechecker.TCUtils(TCError(..)) where

import Parser.AbsTinyCamiot
import Parser.PrintTinyCamiot

import Data.List

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
  | LambdaConstError (Const ())
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