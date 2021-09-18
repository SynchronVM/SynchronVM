module DetectRecs.DetectRecs where

import Control.Monad (join)
import Data.Foldable
import Data.Graph
import Desugaring.AST
import Debug.Trace
import qualified Data.Map as Map
import qualified Parser.AbsTinyCamiot as A

import Data.Graph (Graph)
import qualified Data.Graph as Graph
import Data.Array


type Bimap = [(A.Ident, Int)]

lookupl :: A.Ident -> Bimap -> Int
lookupl _ [] = error "Identifier not found"
lookupl id ((x,y):xs)
  | id == x = y
  | otherwise = lookupl id xs

lookupR :: Int -> Bimap -> A.Ident
lookupR _ [] = error "Int not found"
lookupR int ((x,y):xs)
  | int == y = x
  | otherwise = lookupR int xs

detectRecs :: SExp SType -> SExp SType
detectRecs e = rewriteLets cycIds e
  where
    adjlist = detectCalls empty e
    cycIds  = getCyclicIds adjlist


makeGraph :: [(Int, [Int])] -> Graph
makeGraph list =
  array (minimum nodes, maximum nodes) list
  where
    nodes = map fst list



genbimap :: AdjacencyList -> Bimap
genbimap al = zip ks [0..(length ks - 1)]
  where
    ks = Map.keys al :: [A.Ident]

getInt :: Bimap -> A.Ident -> Int
getInt bmap var = lookupl var bmap

getIdent :: Bimap -> Int -> A.Ident
getIdent bmap int = lookupR int bmap


getCyclicIds :: AdjacencyList -> [A.Ident]
getCyclicIds al = map (\i -> getIdent bmap i) $ cyclicNodes $ makeGraph al''
  where
    bmap = genbimap al
    al'  = Map.toList al :: [(A.Ident,[A.Ident])]
    al'' =
      map (\(x,y) -> (getInt bmap x, map (getInt bmap) y)) al' :: [(Int, [Int])]


cyclicNodes :: Graph -> [Int]
cyclicNodes graph =
  map fst . filter isCyclicAssoc . assocs $ graph
  where
    isCyclicAssoc = uncurry $ reachableFromAny graph

reachableFromAny :: Graph -> Int -> [Int] -> Bool
reachableFromAny graph node =
  elem node . concatMap (Graph.reachable graph)




rewriteLets :: [A.Ident] -> SExp SType -> SExp SType
-- rewriteLets ids (SELet ty1
--                  (SPVar ty2 (A.Ident var1)) (SEVar ty3 (A.Ident var2))
--                  ein)
--   = (SELet ty1
--       (SPVar ty2 (A.Ident var1)) (SEVar ty3 (A.Ident var2))
--       (rewriteLets ids ein))
rewriteLets ids (SELet ty1 (SPVar ty2 (A.Ident var)) ebound ein)
  | (A.Ident var) `elem` ids =
    (SELetR ty1 (SPVar ty2 (A.Ident var))
     (rewriteLets ids ebound)
     (rewriteLets ids ein))
  | otherwise =
    (SELet ty1 (SPVar ty2 (A.Ident var))
     (rewriteLets ids ebound)
     (rewriteLets ids ein))
rewriteLets ids (SELet ty p ebound ein) =
  SELet ty p (rewriteLets ids ebound) (rewriteLets ids ein)
rewriteLets ids (SELetR ty p ebound ein) =
  SELetR ty p (rewriteLets ids ebound) (rewriteLets ids ein)
rewriteLets ids (SECase ty exp pats) =
  SECase ty (rewriteLets ids exp) pats'
  where
    pats' = map (\(SPM ty e) -> (SPM ty (rewriteLets ids e))) pats
rewriteLets ids (SELam ty p e) =
  SELam ty p (rewriteLets ids e)
rewriteLets ids (SEIf ty cond thn els) =
  SEIf ty (rewriteLets ids cond) (rewriteLets ids thn) (rewriteLets ids els)
rewriteLets ids (SEApp ty e1 e2) =
  SEApp ty (rewriteLets ids e1) (rewriteLets ids e2)
rewriteLets ids (SEOr ty e1 e2) =
  SEOr ty (rewriteLets ids e1) (rewriteLets ids e2)
rewriteLets ids (SEAnd ty e1 e2) =
  SEAnd ty (rewriteLets ids e1) (rewriteLets ids e2)
rewriteLets ids (SERel ty e1 relop e2) =
  SERel ty (rewriteLets ids e1) relop (rewriteLets ids e2)
rewriteLets ids (SEAdd ty e1 addop e2) =
  SEAdd ty (rewriteLets ids e1) addop (rewriteLets ids e2)
rewriteLets ids (SEMul ty e1 mulop e2) =
  SEMul ty (rewriteLets ids e1) mulop (rewriteLets ids e2)
rewriteLets ids (SETup ty e1 e2) =
  SETup ty (rewriteLets ids e1) (rewriteLets ids e2)
rewriteLets ids (SENot ty e) =
  SENot ty (rewriteLets ids e)
rewriteLets _ e = e





type AdjacencyList =  Map.Map A.Ident [A.Ident]


empty :: AdjacencyList
empty = Map.empty

detectCalls :: AdjacencyList -> SExp SType -> AdjacencyList
detectCalls al (SELet _ (SPVar _ (A.Ident var1)) (SEVar _ (A.Ident var2)) ein) =
  detectCalls al' ein
  where
    al' = Map.insert (A.Ident var1) [(A.Ident var2)] al

detectCalls al (SELet _ (SPVar _ (A.Ident var)) ebound ein) =
  detectCalls al' ebound `Map.union` detectCalls al' ein
  where
    al' = Map.insert (A.Ident var) (findCallSites ebound) al
detectCalls al (SELet _ _ ebound ein) =
  detectCalls al ebound `Map.union` detectCalls al ein
detectCalls al (SELetR _ _ ebound ein) =
  Map.union (detectCalls al ebound) (detectCalls al ein)
detectCalls al (SECase _ exp pats) =
  Map.union (detectCalls al exp) othermap
  where
    othermap = fold $ map (\(SPM _ e) -> detectCalls al e) pats
detectCalls al (SELam _ _ e) = detectCalls al e
detectCalls al (SEIf _ cond thn els) =
  detectCalls al cond `Map.union`
  detectCalls al thn  `Map.union`
  detectCalls al els
detectCalls al (SEApp _ e1 e2) =
  detectCalls al e1 `Map.union` detectCalls al e2
detectCalls al (SEOr _ e1 e2) = detectCalls al e1 `Map.union` detectCalls al e2
detectCalls al (SEAnd _ e1 e2) = detectCalls al e1 `Map.union` detectCalls al e2
detectCalls al (SERel _ e1 _ e2) =
  detectCalls al e1 `Map.union` detectCalls al e2
detectCalls al (SEAdd _ e1 _ e2) =
  detectCalls al e1 `Map.union` detectCalls al e2
detectCalls al (SEMul _ e1 _ e2) =
  detectCalls al e1 `Map.union` detectCalls al e2
detectCalls al (SETup _ e1 e2) = detectCalls al e1 `Map.union` detectCalls al e2
detectCalls al (SENot _ e) = detectCalls al e
detectCalls al _ = al


findCallSites :: SExp SType -> [A.Ident]
findCallSites (SEApp _ (SEVar _ (A.Ident var)) e2) =
  (A.Ident var) : findCallSites e2
findCallSites (SEApp _ e1 e2) =
  findCallSites e1 ++ findCallSites e2
findCallSites (SECase _ caseExp pats) =
  findCallSites caseExp ++
  (join $ map (\(SPM _ e) -> findCallSites e) pats)
findCallSites (SELet _ _ e1 e2) =
  findCallSites e1 ++ findCallSites e2
findCallSites (SELetR _ _ e1 e2) =
  findCallSites e1 ++ findCallSites e2
findCallSites (SELam _ _ e) = findCallSites e
findCallSites (SEIf _ cond thn els) =
  findCallSites cond ++ findCallSites thn ++ findCallSites els
findCallSites (SEOr _ e1 e2) = findCallSites e1 ++ findCallSites e2
findCallSites (SEAnd _ e1 e2) = findCallSites e1 ++ findCallSites e2
findCallSites (SERel _ e1 _ e2) = findCallSites e1 ++ findCallSites e2
findCallSites (SEAdd _ e1 _ e2) = findCallSites e1 ++ findCallSites e2
findCallSites (SEMul _ e1 _ e2) = findCallSites e1 ++ findCallSites e2
findCallSites (SETup _ e1 e2) = findCallSites e1 ++ findCallSites e2
findCallSites (SENot _ e) = findCallSites e
findCallSites _ = []

foo = SELet (STLam STInt STInt) (SPVar (STLam STInt STInt) (A.Ident "v5")) (SELam (STLam STInt STInt) (SPVar STInt (A.Ident "v6")) (SECase STInt (SEVar STInt (A.Ident "v6")) [SPM (SPVar STInt (A.Ident "v4")) (SEApp STInt (SEVar (STLam STInt STInt) (A.Ident "v0")) (SEVar STInt (A.Ident "v4")))])) (SELet (STLam STInt STInt) (SPVar (STLam STInt STInt) (A.Ident "v0")) (SELam (STLam STInt STInt) (SPVar STInt (A.Ident "v7")) (SECase STInt (SEVar STInt (A.Ident "v7")) [SPM (SPVar STInt (A.Ident "v1")) (SELet STInt (SPVar (STLam STInt STInt) (A.Ident "v2")) (SEVar (STLam STInt STInt) (A.Ident "v5")) (SEApp STInt (SEVar (STLam STInt STInt) (A.Ident "v2")) (SEConst STInt (A.CInt 3))))])) (SEApp STInt (SEVar (STLam STInt STInt) (A.Ident "v0")) (SEConst STInt (A.CInt 2))))

zzz = (SELam (STLam STInt STInt) (SPVar STInt (A.Ident "v6")) (SECase STInt (SEVar STInt (A.Ident "v6")) [SPM (SPVar STInt (A.Ident "v4")) (SEApp STInt (SEVar (STLam STInt STInt) (A.Ident "v0")) (SEVar STInt (A.Ident "v4")))]))
