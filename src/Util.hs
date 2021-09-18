module Util(
  topologicalSort,
  groupStrs
) where

import qualified Newtype.IntSet as IntSet
import Data.Type.Coercion.Sub

topologicalSort :: Sub k Int -> k -> (k -> v) -> (v -> [k]) -> [(k,v)]
topologicalSort keyIsInt root nodeOf childrenOf = snd $ visit (IntSet.empty keyIsInt) [] [root]
  where
    visit visited acc [] = (visited, acc)
    visit visited acc (x : rest)
      | x `IntSet.member` visited = visit visited acc rest
      | otherwise =
           let node = nodeOf x
               visited' = IntSet.insert x visited
               (visited'', acc') = visit visited' acc (childrenOf node)
               acc'' = (x,node) : acc'
           in visit visited'' acc'' rest

groupStrs :: (Eq c) => [[c]] -> (Bool, [(c,[[c]])])
groupStrs = foldr step (False, [])
  where
    step [] (_,es)     = (True, es)
    step (c:cs) (a,es) = case es of
      ((d,dss) : rest)
        | c == d -> (a, (d, cs : dss) : rest)
      _ -> (a, (c, [cs]) : es)
