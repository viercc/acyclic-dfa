module Data.ADFA.Util(
  topologicalSort,
  groupStrs
) where

import qualified Data.ADFA.IdSet       as IdSet
import           Data.ADFA.NodeId      (NodeId)

topologicalSort :: NodeId s -> (NodeId s -> v) -> (v -> [NodeId s]) -> [(NodeId s,v)]
topologicalSort root nodeOf childrenOf = snd $ visit IdSet.empty [] [root]
  where
    visit visited acc [] = (visited, acc)
    visit visited acc (x : rest)
      | x `IdSet.member` visited = visit visited acc rest
      | otherwise =
           let node = nodeOf x
               visited' = IdSet.insert x visited
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
