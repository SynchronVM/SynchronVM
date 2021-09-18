{-# LANGUAGE FlexibleInstances #-}
module Generator where

import CamIoT.Internal.Syntax

import Test.QuickCheck


instance Arbitrary (Exp ()) where
    arbitrary = undefined
