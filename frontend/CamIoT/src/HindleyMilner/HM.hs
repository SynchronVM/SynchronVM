{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module HindleyMilner.HM 
       ( 
         Unify(..)
       , runUnify
       , UniError(..)
       ) where

import Parser.PrintTinyCamiot

data UniError = UniError String

class Print a => Unify a where
    type Proof a
    trivial :: Proof a
    unify   :: a -> a -> Either UniError (Proof a)
    refine  :: Proof a -> a -> a
    compose :: Proof a -> Proof a -> Proof a

instance Unify a => Unify [a] where
    type Proof [a] = Proof a
    trivial = trivial @a
    
    unify [] []         = return $ trivial @a
    unify (a:as) (b:bs) = do
        su1 <- unify a b
        su2 <- unify (refine su1 as) (refine su1 bs)
        return $ compose @a su2 su1
    unify a b = Left $ UniError $ "cannot unify: " ++ printTree a ++ " with " ++ printTree b

    refine p = map (refine p)

    compose = compose @a

refinePair :: Unify a => Proof a -> (a, a) -> (a,a)
refinePair proof (x,y) = (refine proof x, refine proof y)

unifyOneOf :: forall a . Unify a => [(a,a)] -> Either UniError (Proof a)
unifyOneOf []           = return $ trivial @a
unifyOneOf [(c1,c2)]    = unify c1 c2
unifyOneOf ((c1,c2):cs) = case unify @a  c1 c2 of
    Right s -> Right s
    Left e  -> unifyOneOf cs

{-- | If you have collected a list of constraints, try to solve them one by one and return
a proof if unification succeeded.
-}
runUnify :: forall a. Unify a => [[(a,a)]] -> Either UniError (Proof a)
runUnify a = runUnify' (a, trivial @a)
  where
      {-- | Tries to unify a list of constraints. If successful, the proof will be
      combined with the 'current' proof su, and the following constraints will be
      refined before they are recursively unified themselves.
      -}
      runUnify' :: forall a. Unify a => ([[(a,a)]], Proof a) -> Either UniError (Proof a)
      runUnify' ([], su) = return su
      runUnify' (c:cs, su) = case c of
              [(a1,a2)] -> do
                  su1 <- unify @a a1 a2
                  let cs' = map (map (refinePair su1)) cs
                  runUnify' (cs', compose @a su1 su)
              oneof     -> do
                  su1 <- unifyOneOf @a oneof
                  let cs' = map (map (refinePair su1)) cs
                  runUnify' (cs', compose @a su1 su)
