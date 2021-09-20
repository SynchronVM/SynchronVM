module CamIoT.Typecheck where

import           CamIoT.Internal.Syntax
import           CamIoT.Typecheck.Environment
import           CamIoT.Typecheck.Substitution
import           CamIoT.Typecheck.Syntax

import qualified Data.Map                      as Map

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

{- | Typecheck a program. The input is

  1. Initial identifier environment, e.g the types of primitive functions
  2. Initial constructor environment, e.g the types of primitive ADTs (lists, perhaps?)
  3. The program to typecheck

The result is either an error that occured during typechecking, or a tuple consisting of

  1. A substitution that maps type variables to types
  2. The input program, but annotated with type information.

The substitution can be applied to the program to yield the finished, typechecked
program. -}
typecheck
  :: Map.Map Ident Schema
  -> Map.Map UIdent Schema
  -> Program ()
  -> Either String (Subst, Program Type)
typecheck initialFunctions initialConstructors p =
  case
      runReader (evalStateT (runExceptT p') stateMonadState) readerMonadState
    of
      Left  e -> Left $ show e
      Right r -> Right r
 where
  p' = checkProgram p

  readerMonadState :: Env
  readerMonadState = Env initialFunctions initialConstructors

  stateMonadState :: TCState
  stateMonadState = TCState 0 tyconArities

  tyconArities :: Map.Map UIdent Int
  tyconArities =
    Map.map (length . args . getTypeWithoutInstantiate) initialConstructors
