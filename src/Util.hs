module Util where

import qualified Data.Set        as Set

-- | Two inputs must be sorted in ascending order.
trisectList :: (Ord k) =>
  [(k,a)] -> [(k,b)] -> ([(k,a)], [(k,(a,b))], [(k,b)])
trisectList [] bs = ([], [], bs)
trisectList as [] = (as, [], [])
trisectList as@((ka, a):as') bs@((kb,b):bs') =
  case compare ka kb of
    LT -> case trisectList as bs' of
      (onlyA, both, onlyB) -> ((ka,a) : onlyA, both, onlyB)
    EQ -> case trisectList as' bs' of
      (onlyA, both, onlyB) -> (onlyA, (ka,(a,b)) : both, onlyB)
    GT -> case trisectList as' bs of
      (onlyA, both, onlyB) -> (onlyA, both, (kb,b) : onlyB)

topologicalSort :: (Ord k) => k -> (k -> v) -> (v -> [k]) -> [(k,v)]
topologicalSort root nodeOf childrenOf = snd $ visit Set.empty [] [root]
  where
    visit visited acc [] = (visited, acc)
    visit visited acc (x : rest)
      | x `Set.member` visited = visit visited acc rest
      | otherwise =
           let node = nodeOf x
               visited' = Set.insert x visited
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
