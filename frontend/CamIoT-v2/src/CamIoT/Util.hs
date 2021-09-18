{- | This module implements some general utility functions and datatypes that can be
used in any phase of the compiler. -}
module CamIoT.Util where

import           Control.Monad.State

import           CamIoT.Internal.Syntax

{- | Class of types that can produce and consume an integer. Used in conjunction with
`MonadState a m` where `a` has an `IntState` constraint. We can use this to reuse
e.g the `freshIdentifier` function with many different type of states, as long as we
can figure out how to get an int from the state and how to write an int to the state. -}
class IntState a where
  getInt :: a -> Int
  setInt :: Int -> a -> a

-- | Trivial instance for a simple integer state
instance IntState Int where
  getInt = id
  setInt i _ = i

-- | Generate a fresh identifier
freshIdentifier :: (IntState a, MonadState a m) => m Ident
freshIdentifier = do
  st <- get
  let i = getInt st
  put $ setInt (i + 1) st
  return $ Ident $ "v" ++ show i

{- | Utility function that performs a monadic computation if the first monadic
result is @True@. If not, the default value (third argument) is returned. -}
whenM :: Monad m => m Bool -> m a -> a -> m a
whenM mb ma d = do
  b <- mb
  if b then ma else return d
