module Util where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import qualified Queue           as Q

trisect :: (Ord k) => Map k a -> Map k b -> (Map k a, Map k (a,b), Map k b)
trisect ma mb = (Map.difference ma mb,
                 Map.intersectionWith (,) ma mb,
                 Map.difference mb ma)

topologicalSort :: (Ord k) => k -> (k -> v) -> (v -> [k]) -> [(k,v)]
topologicalSort root nodeOf childrenOf =
  snd . Q.traceQueue $
    do Q.offer [(root, nodeOf root)]
       loop (Set.singleton root)
  where
    loop subst = Q.poll 1 >>= \q ->
      case q of
        []        -> return ()
        ((_,v):_) -> expand v subst >>= loop
    
    expand v subst =
      do Q.offer ys
         return subst'
      where
        js = (`Set.difference` subst) . Set.fromList $ childrenOf v
        subst' = Set.union subst js
        ys = map (\j -> (j, nodeOf j)) $ Set.toList js

topologicalSort' :: (Ord k) => k -> (k -> v) -> (v -> [k]) -> (Map k Int, [(k,v)])
topologicalSort' root nodeOf childrenOf =
  Q.traceQueue $
    do Q.offer [(root, nodeOf root)]
       loop (Map.singleton root 0)
  where
    loop subst = Q.poll 1 >>= \q ->
      case q of
        []        -> return subst
        ((_,v):_) -> expand v subst >>= loop
    
    expand v subst =
      do Q.offer ys
         return subst'
      where
        n = Map.size subst
        js = Set.fromList . filter (`Map.notMember` subst) $ childrenOf v
        substJ = snd . Map.mapAccum s n $ Map.fromSet (const ()) js
        s x () = x `seq` (x+1, x)
        subst' = Map.union subst substJ
        ys = map (\j -> (j, nodeOf j)) $ Set.toList js

groupStrs :: (Eq c) => [[c]] -> (Bool, [(c,[[c]])])
groupStrs = foldr step (False, [])
  where
    step [] (_,es)     = (True, es)
    step (c:cs) (a,es) = case es of
      ((d,dss) : rest)
        | c == d -> (a, (d, cs : dss) : rest)
      _ -> (a, (c, [cs]) : es)