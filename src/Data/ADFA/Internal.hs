{-| Internals for Data.ADFA -}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.ADFA.Internal(
  ADFA(..),
  Node(..),
  NodeId(..),
  empty, string, strings,
  instantiate,
  treeInstantiate,
  toTable, fromTable,
  -- * low-level running
  accepts,
  step, steps, match,
  -- * Debug
  debugShow, debugPrint,
  -- * Internals
  (!), (!>), renumber,
) where

import           Data.Semigroup
import           Data.Maybe (fromMaybe)
import           Data.Foldable (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import qualified Data.Vector          as V
import qualified Data.Vector.Growable as GV

import           Data.Bifunctor
import           Control.DeepSeq

import           Data.Functor.Classes

data ADFA c =
  forall s. MkDFA
    { getNodes   :: !(V.Vector (Node c (NodeId s)))
    , rootNode   :: !(NodeId s) }

deriving instance Show c => Show (ADFA c)

instance Eq c => Eq (ADFA c) where
  MkDFA nodesA rootA == MkDFA nodesB rootB =
    liftEq (liftEq nodeEq) nodesA nodesB &&
    getNodeId rootA == getNodeId rootB
    where
      nodeEq (NodeId x) (NodeId y) = x == y

instance Ord c => Ord (ADFA c) where
  MkDFA nodesA rootA `compare` MkDFA nodesB rootB =
    liftCompare (liftCompare nodeCompare) nodesA nodesB <>
    getNodeId rootA `compare` getNodeId rootB
    where
      nodeCompare (NodeId x) (NodeId y) = compare x y

instance NFData c => NFData (ADFA c) where
  rnf (MkDFA nodes _) = rnf nodes

data Node c r = Node { isAccepted :: !Bool, outEdges :: !(Map c r) }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance (NFData c, NFData r) => NFData (Node c r) where
  rnf (Node _ e) = rnf e

instance (Eq c) => Eq1 (Node c) where
  liftEq eq (Node accA edgesA) (Node accB edgesB) =
    accA == accB && liftEq eq edgesA edgesB

instance (Ord c) => Ord1 (Node c) where
  liftCompare cmp (Node accA edgesA) (Node accB edgesB) =
    compare accA accB <> liftCompare cmp edgesA edgesB

-- Node Id
newtype NodeId s = NodeId { getNodeId :: Int }
  deriving (Eq, Ord, Enum, Num, NFData)

instance Show (NodeId s) where
  show (NodeId n) = show n

instance Read (NodeId s) where
  readsPrec p s = first NodeId <$> readsPrec p s

-- | Empty ADFA which accepts no string.
empty :: ADFA c
empty = MkDFA nodes 0
  where nodes = V.fromList [Node False Map.empty]

-- | Construct from a string.
string :: [c] -> ADFA c
string cs = MkDFA nodes 0
  where makeNode i c = Node False (Map.singleton c (i+1))
        nodes = V.fromList $ zipWith makeNode [0..] cs ++ [Node True Map.empty]

-- | Construct from strings. Resulted ADFA has tree-like structure,
--   which might not be the most compact representation.
strings :: (Ord c) => [[c]] -> ADFA c
strings css = MkDFA nodes 0
  where
    node0 = Node False Map.empty
    nodes = V.create $ do
      vnodes <- GV.new 1
      GV.unsafeWrite vnodes 0 node0
      mapM_ (insert vnodes 0) css
      GV.unsafeToMVector vnodes
    
    insert vnodes x [] =
      GV.modify vnodes (\node -> node{isAccepted = True}) x
    insert vnodes x (c:cs) = do
      Node accX edgesX <- GV.unsafeRead vnodes x
      case Map.lookup c edgesX of
        Nothing -> do
          y <- GV.length vnodes
          let node' = Node accX $ Map.insert c (NodeId y) edgesX
          GV.grow 1 vnodes
          GV.unsafeWrite vnodes x node'
          GV.unsafeWrite vnodes y node0
          insert vnodes y cs
        Just (NodeId y) ->
          insert vnodes y cs

instantiate :: (Ord k) => k -> (k -> Node c k) -> ADFA c
instantiate rootKey stepKey = MkDFA nodes 0
  where
    nodes = V.create $ do
      vnodes <- GV.new 0
      _ <- assign vnodes rootKey Map.empty
      GV.unsafeToMVector vnodes
    
    assign vnodes key subst =
      case Map.lookup key subst of
        Just i -> return (i, subst)
        Nothing ->
          do x <- GV.length vnodes
             GV.grow 1 vnodes
             let subst' = Map.insert key (NodeId x) subst
                 Node t e = stepKey key
             (e', subst'') <- advance vnodes e subst'
             GV.unsafeWrite vnodes x (Node t e')
             return (NodeId x, subst'')
    
    advance vnodes edges subst' =
      do (es', subst'') <- advanceLoop vnodes [] (Map.toAscList edges) subst'
         return (Map.fromDistinctDescList es', subst'')
    
    advanceLoop _      acc []           subst = return (acc, subst)
    advanceLoop vnodes acc ((c,k):rest) subst =
      do (y, subst') <- assign vnodes k subst
         advanceLoop vnodes ((c,y):acc) rest subst'

treeInstantiate :: k -> (k -> Node c k) -> ADFA c
treeInstantiate rootKey stepKey = MkDFA nodes 0
  where
    nodes = V.create $ do
      vnodes <- GV.new 0
      _ <- assign vnodes rootKey
      GV.unsafeToMVector vnodes
    
    assign vnodes key =
      do x <- GV.length vnodes
         GV.grow 1 vnodes
         node <- traverse (assign vnodes) (stepKey key)
         GV.unsafeWrite vnodes x node
         return (NodeId x)

toTable :: ADFA c -> (Int, [(Int, Node c Int)])
toTable (MkDFA nodes root) =
  let NodeId rootI = root
      nodeListI = map (fmap getNodeId) (V.toList nodes)
  in (rootI, zip [0..] nodeListI)

fromTable :: Ord k => k -> [(k, Node c k)] -> Maybe (ADFA c)
fromTable root nodes =
  if noUndefinedKeys && isAcyclic nodeGraph
    then Just $ renumber root nodes
    else Nothing
  where
    nodeMap = Map.fromList nodes
    nodeGraph = Map.map (Map.elems . outEdges) nodeMap
    destinations =
      [ k | (_,Node _ edges) <- nodes,
            (_,k) <- Map.toList edges ]
    noUndefinedKeys = Map.member root nodeMap &&
      all (`Map.member` nodeMap) destinations

isAcyclic :: (Ord k) => Map k [k] -> Bool
isAcyclic g = go (Map.keys g) Map.empty
  where
    go [] _ = True
    go (k:rest) reach =
      let reach' = reachability reach k
      in case Map.lookup k reach' of
           Nothing -> go rest reach'
           Just rs -> Set.notMember k rs && go rest reach'
    
    reachability reach k
      | Map.member k reach = reach
      | otherwise =
          let children = fromMaybe [] $ Map.lookup k g
              reach' = foldl' reachability reach children
              f accum j =
                let descendants = fromMaybe Set.empty $ Map.lookup j reach'
                in accum `Set.union` descendants
              rs0 = Set.fromList children
              rs = foldl' f rs0 children
          in Map.insert k rs reach'

-- | Is given node accepted state?
accepts :: V.Vector (Node c (NodeId s)) -> NodeId s -> Bool
accepts dfa x = isAccepted $ dfa ! x

-- | Runs an ADFA one step for given input character.
step :: (Ord c) => V.Vector (Node c (NodeId s)) -> c -> NodeId s -> Maybe (NodeId s)
step dfa c x = Map.lookup c $ dfa !> x

-- | Runs an ADFA for given string of input alphabet.
steps :: (Ord c) => V.Vector (Node c (NodeId s)) -> [c] -> NodeId s -> Maybe (NodeId s)
steps dfa = loop
 where
  loop []     x = Just x
  loop (c:cs) x = step dfa c x >>= loop cs

-- | Decides if an ADFA accepts a string.
match :: (Ord c) => ADFA c -> [c] -> Bool
match (MkDFA nodes root) cs =
  case steps nodes cs root of
    Nothing -> False
    Just x  -> nodes `accepts` x

-- * Debug
debugPrint :: (Show c) => ADFA c -> IO ()
debugPrint = putStrLn . debugShow

debugShow :: (Show c) => ADFA c -> String
debugShow (MkDFA nodes root) =
  unlines $ concatMap oneNode (zip [0..] (V.toList nodes))
  where
    showNodePretty x t =
      let rootIndicator = if x == root then "r" else " "
          typeIndicator = if t then "a" else " "
      in show x ++ rootIndicator ++ typeIndicator
    oneNode (x, Node t nexts) = showNodePretty x t : map ((" " ++) . oneEdge) (Map.toAscList nexts)
    oneEdge (c, x') = show c ++ "->" ++ show x'

-- * Unsafe operations

-- | Unsafe indexing
(!) :: V.Vector (Node c (NodeId s)) -> NodeId s -> Node c (NodeId s)
(!) nodes (NodeId x) = nodes V.! x

(!>) :: V.Vector (Node c (NodeId s)) -> NodeId s -> Map c (NodeId s)
(!>) nodes x = outEdges (nodes ! x)

renumber :: (Ord k) => k -> [(k, Node c k)] -> ADFA c
renumber rootX sortedNodes = MkDFA nodes rootY
  where
    xs = map fst sortedNodes
    subst = Map.fromList $ zip xs [0..]
    n = Map.size subst
    
    applySubst = fmap (subst Map.!)
    rootY = subst Map.! rootX
    nodes = V.fromListN n $ map (applySubst . snd) sortedNodes
