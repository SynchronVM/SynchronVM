module Monomorphisation.Monomorphise where

import Parser.AbsTinyCamiot

import qualified Data.Map as Map

monomorphise :: [Def Type] -> [Def Type]
monomorphise = undefined

fetchPolymorphicFunctions :: [Def Type] -> [Def Type]
fetchPolymorphicFunctions = undefined

fetchApplications :: [Def Type] -> Map.Map Ident [Type]
fetchApplications = undefined

-- and so forth