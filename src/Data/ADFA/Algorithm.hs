{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
module Data.ADFA.Algorithm(
  -- * Query
  enumerate, stringCount, isEmpty, equivalent,
  foldNodes, foldNodes',
  -- * Build
  string, strings,
  -- * Combine
  union, intersection, difference, append,
  -- * Other operations
  prefixes, suffixes, infixes,
  -- * Cleaning ADFA without changing its semantics
  topSort, prune, minify,
  -- * Conversion
  fromList, toList,
  fromAscList, toAscList,
  fromSet, toSet
) where

import           Data.Tuple          (swap)
import qualified Data.Map.Lazy       as LMap
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Vector         as V

import qualified Control.Applicative
import           Control.Monad.State

import           Data.ADFA.Internal
import           Util

-- | Enumerates all unique strings an ADFA accepts.
--
-- > strings . enumerate = id
enumerate :: ADFA c -> [[c]]
enumerate = foldNodes f
  where
    f acc nexts =
      let nil = [ [] | acc ]
          advance = [ c:cs | (c, css) <- Map.toAscList nexts, cs <- css ]
      in nil ++ advance

isEmpty :: ADFA c -> Bool
isEmpty = foldNodes f
  where
    f acc nexts = not acc && and nexts

-- | Counts the number of all unique strings an ADFA accepts.
stringCount :: ADFA c -> Int
stringCount = foldNodes' f
  where
    f acc nexts = (if acc then 1 else 0) + sum nexts

-- | Equivalence
equivalent :: (Ord c) => ADFA c -> ADFA c -> Bool
equivalent (prune -> MkDFA nodesA rootA) (prune -> MkDFA nodesB rootB) =
  eqv (rootA, rootB)  
  where eqv (a,b) =
          let Node acceptsA edgesA = nodesA ! a
              Node acceptsB edgesB = nodesB ! b
              (onlyA, both, onlyB) =
                trisectList (Map.toAscList edgesA)
                            (Map.toAscList edgesB)
          in acceptsA == acceptsB &&
             null onlyA &&
             null onlyB &&
             all (eqv . snd) both

-----------------------------------------------------

union :: (Ord c) => ADFA c -> ADFA c -> ADFA c
union (MkDFA nodesA rootA) (MkDFA nodesB rootB) = instantiate rootKey stepKey
  where
    rootKey = (Just rootA, Just rootB)
    
    node0 = Node False Map.empty

    stepKey (mayA, mayB) =
      let Node acceptsA edgesA = maybe node0 (nodesA !) mayA
          Node acceptsB edgesB = maybe node0 (nodesB !) mayB
      in Node (acceptsA || acceptsB) (merge edgesA edgesB)
    
    merge :: (Ord k) => Map k a -> Map k b -> Map k (Maybe a, Maybe b)
    merge as bs =
      let f (ma, _) (_, mb) = (ma, mb)
          as' = (\a -> (Just a, Nothing)) <$> as
          bs' = (\b -> (Nothing, Just b)) <$> bs
      in Map.unionWith f as' bs'

-- | Constructs an ADFA which accepts a string iff both ADFAs accept it.
intersection :: Ord c => ADFA c -> ADFA c -> ADFA c
intersection (MkDFA nodesA rootA) (MkDFA nodesB rootB) = instantiate rootKey stepKey
  where
    rootKey = (rootA, rootB)
    stepKey (a, b) =
      let Node acceptsA edgesA = nodesA ! a
          Node acceptsB edgesB = nodesB ! b
      in Node (acceptsA && acceptsB) (Map.intersectionWith (,) edgesA edgesB)

difference :: (Ord c) => ADFA c -> ADFA c -> ADFA c
difference (MkDFA nodesA rootA) (MkDFA nodesB rootB) = instantiate rootKey stepKey
  where
    rootKey = (rootA, Just rootB)
    
    stepKey (a, Just b) =
      let Node acceptsA edgesA = nodesA ! a
          acceptsKey = acceptsA && not (nodesB `accepts` b)
          edges = Map.mapWithKey (\c a' -> (a', step nodesB c b)) edgesA
      in Node acceptsKey edges
    stepKey (a, Nothing) =
      let Node acceptsA edgesA = nodesA ! a
          edges = Map.map (\a' -> (a', Nothing)) edgesA
      in Node acceptsA edges

append :: (Ord c) => ADFA c -> ADFA c -> ADFA c
append (MkDFA nodesA rootA) (MkDFA nodesB rootB) = instantiate rootKey stepKey
  where
    rootKey = (Just rootA, Set.empty)
    
    node0 = Node False Map.empty
    
    (<|>) = (Control.Applicative.<|>)
    unionKey (mayA1, bs1) (mayA2, bs2) = (mayA1 <|> mayA2, bs1 `Set.union` bs2)
    fromA a = (Just a, Set.empty)
    fromB b = (Nothing, Set.singleton b)
    
    rootNodeB = nodesB ! rootB
    
    stepKey (mayA, bSet) =
      let Node acceptsA edgesA = maybe node0 (nodesA !) mayA
          nodeBs = (nodesB !) <$> Set.toList bSet
          nodeBs' = if acceptsA then rootNodeB : nodeBs else nodeBs
          acceptsKey = any isAccepted nodeBs'
          edgesKey =
            Map.unionsWith unionKey $
              Map.map fromA edgesA :
              map (Map.map fromB . outEdges) nodeBs'
      in Node acceptsKey edgesKey

-- | Accept all prefixes of currently accepted strings.
prefixes :: ADFA c -> ADFA c
prefixes (MkDFA nodes root) = MkDFA nodes' root
  where
    nodes' = snd <$> traversalTable prefixes' nodes
    prefixes' (Node acceptsX edgesX) =
      let edgesX' = snd <$> edgesX
          nonEmptyX = acceptsX || any fst edgesX
      in (nonEmptyX, Node nonEmptyX edgesX')

-- | Accepts all suffixes of currently accepted strings.
suffixes :: (Ord c) => ADFA c -> ADFA c
suffixes (MkDFA nodes root) = instantiate rootKey stepKey
  where
    reachable accum [] = accum
    reachable accum (x:xs)
      | x `Set.member` accum = reachable accum xs
      | otherwise            =
          let accum' = Set.insert x accum
              ys = Map.elems $ nodes !> x
          in reachable accum' (ys ++ xs)
    
    rootKey = reachable Set.empty [root]
    stepKey xs = 
      let node0 = Node False Map.empty
          merge (Node acceptsKey edgesKey) x =
            let Node acceptsX edgesX = nodes ! x
                acceptsKey' = acceptsKey || acceptsX
                edgesX' = Map.map Set.singleton edgesX
                edgesKey' = Map.unionWith Set.union edgesKey edgesX'
            in Node acceptsKey' edgesKey'
      in Set.foldl' merge node0 xs

-- | Accepts all contiguous substrings of currently accepted strings.
infixes :: (Ord c) => ADFA c -> ADFA c
infixes = suffixes . prefixes

-- | Relabel nodes to make all paths have increasing order.
--   Applying @topSort@ also eliminates unreachable nodes.
topSort :: ADFA c -> ADFA c
topSort (MkDFA nodes root) =
  renumber root $ topologicalSort root (nodes !) (Map.elems . outEdges)

-- | Remove empty nodes which is not accept node and only goes to
--   other empty nodes.
prune :: ADFA c -> ADFA c
prune (MkDFA nodes root) = if rootIsEmpty then empty else dfa'
  where
    table = traversalTable pruneStep nodes

    pruneStep (Node acceptsX edgesX) =
      let edgesX' = Map.mapMaybe id edgesX
          isEmptyX = not acceptsX && Map.null edgesX'
      in if isEmptyX then Nothing else Just (Node acceptsX edgesX')
    
    rootIsEmpty = case table V.! getNodeId root of
      Nothing -> True
      Just _ -> False

    dfa' = renumber root
      [ (x,node) | (x, Just node) <- zip [0..] (V.toList table) ]

type ReverseIndex s c = Map (Node c (NodeId s)) (NodeId s)

-- | Minimizes an ADFA by removing redundant nodes.
--   Applying @minify@ also removes unreachable nodes,
--   so there is no need to 'prune' right before or after 'minify'.
minify :: forall c. (Ord c) => ADFA c -> ADFA c
minify (MkDFA nodes root) =
  postprocess $ execState (go root) (dup0, subst0)
  where
    subst0 = Map.empty
    dup0 = Map.empty
    
    addNode :: NodeId s -> Node c (NodeId s) -> ReverseIndex s c ->
                (NodeId s, ReverseIndex s c)
    addNode x node dup =
      case Map.insertLookupWithKey (\_ _ x0 -> x0) node x dup of
        (Nothing, dup') -> (x, dup')
        (Just x0, dup') -> (x0, dup')
    
    go x = do
      (_, subst) <- get
      case Map.lookup x subst of
        Nothing -> do
          let Node accepted neighbours = nodes ! x
          neighbours' <- traverse go neighbours
          let neighbours'' = Map.mapMaybe id neighbours'
              getRemoved = Map.null neighbours''  && not accepted
          if getRemoved
            then do
              modifySnd (Map.insert x Nothing)
              return Nothing
            else do
              x0 <- stateFst (addNode x (Node accepted neighbours''))
              modifySnd (Map.insert x (Just x0))
              return (Just x0)
        Just r -> return r
    
    postprocess (dup, subst) =
      case Map.lookup root subst of
        Just (Just root') ->
          let table = map swap $ Map.toList dup
          in  renumber root' table
        _ -> empty

-- * Conversion

toList, toAscList :: ADFA c -> [[c]]
toList = enumerate
toAscList = enumerate

fromList :: (Ord c) => [[c]] -> ADFA c
fromList = strings

fromAscList :: (Eq c) => [[c]] -> ADFA c
fromAscList css = treeInstantiate css down
  where down xss = case groupStrs xss of
          (t, gs) -> Node t (Map.fromAscList gs)

toSet :: ADFA c -> Set [c]
toSet = Set.fromDistinctAscList . toAscList

fromSet :: Eq c => Set [c] -> ADFA c
fromSet = fromAscList . Set.toAscList

-- Utilities

-- | Fold Acyclic DFA structure.
foldNodes :: (Bool -> Map c r -> r) -> ADFA c -> r
foldNodes f (MkDFA nodes root) = go root
  where
    go x =
      let Node acceptsX edgesX = nodes ! x
      in f acceptsX (LMap.map go edgesX)

-- | Basically same to @foldNodes@ but perform memoized recursion
--   instead of normal recursion.
foldNodes' :: (Bool -> Map c r -> r) -> ADFA c -> r
foldNodes' f (MkDFA nodes root) = table V.! getNodeId root
  where table = V.map g nodes
        g (Node t e) = f t (LMap.map (\(NodeId i) -> table V.! i) e)

traversalTable ::
  (Functor f) =>
  (Node c (f (NodeId s)) -> f (Node c (NodeId s))) ->
  V.Vector (Node c (NodeId s)) ->
  V.Vector (f (Node c (NodeId s)))
traversalTable f nodes = table
  where
    g (Node t e) = f (Node t (LMap.map queryContext e))
    queryContext x@(NodeId i) = x <$ (table V.! i)
    table = V.map g nodes

modifySnd :: (MonadState (a,b) m) => (b -> b) -> m ()
modifySnd f = get >>= \(a, b) -> let !b' = f b in put (a, b')

stateFst :: (MonadState (a,b) m) => (a -> (x,a)) -> m x
stateFst f = get >>= \(a, b) -> let (x, !a') = f a in put (a', b) >> return x
