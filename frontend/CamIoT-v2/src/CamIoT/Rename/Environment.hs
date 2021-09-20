{- | This module contains datatypes used during the alpha-renaming phase. -}
module CamIoT.Rename.Environment where

import qualified Data.Map                      as Map

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           CamIoT.Internal.Syntax
import           CamIoT.Pretty.Syntax

{- | State maintained while alpha-renaming a program. It maps old identifier names to
new identifier names. -}
type RenameState = Map.Map Ident Ident

{- | Datatype declaring the different types of errors that can arise during alpha
renaming. -}
data RenameError = NotRenamed Ident

-- | Show instance for renaming errors
instance Show RenameError where
  show x = case x of
    NotRenamed id -> concat
      [ "renaming error: asked for the new name of "
      , printTree id
      , " but no new name was found in the renaming environment"
      ]

{- | Alpha renaming monad. It is a State + Reader + Except monad transformer. The state
is used to generate fresh names, the reader environment maps old names to new names and
the except monad is, of course, used for throwing errors. -}
type Rename a = StateT Int (ReaderT RenameState (Except RenameError)) a
