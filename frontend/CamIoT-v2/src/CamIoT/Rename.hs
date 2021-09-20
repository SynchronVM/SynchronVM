{- | This module exposes functionality to alpha rename programs. The process of alpha
renaming a program converts a program from a form where names are not necessarily
unique to one where names are guaranteed to be unique. -}
module CamIoT.Rename where

import           CamIoT.Internal.Syntax
import           CamIoT.Rename.Syntax

import qualified Data.Map                      as Map

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State


{- | Alpha rename a program. The input to this function is

  1. A list of identifier names that should not be renamed
  2. An integer that is used to generate fresh names
  3. The program to alpha rename

The output is either an error or a tuple consisting of

  1. The alpha renamed program
  2. The final state of the integer used to generate fresh names

-}
alphaRename :: [Ident] -> Int -> Program a -> Either String (Program a, Int)
alphaRename doNotRename nameGeneratingState p =
  case runExcept $ runReaderT (runStateT p' nameGeneratingState) renameState of
    Left  err -> Left $ show err
    Right r   -> Right r
 where
  p' = renameProgram p

  renameState :: Map.Map Ident Ident
  renameState = Map.fromList $ map dup doNotRename

  dup :: a -> (a, a)
  dup a = (a, a)
