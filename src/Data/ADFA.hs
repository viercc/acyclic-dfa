{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
module Data.ADFA(
  -- * Type
  ADFA(),
  -- * Query
  enumerate, stringCount, size, isEmpty, equivalent,
  member, notMember,
  advance,
  -- * Build
  empty, string, strings,
  -- * Combine
  union, unions, intersection, difference, symdiff, append,
  -- * Other operations
  prefixes, suffixes, infixes, reverse,
  -- * Cleaning ADFA without changing its semantics
  topSort, prune, minify,
  -- * Conversion
  fromList, toList,
  fromAscList, toAscList,
  fromSet, toSet
) where

import           Prelude             hiding (reverse)

import           Data.List           (foldl')
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Tuple          (swap)

import qualified Newtype.IntMap     as IntMap
import qualified Newtype.IntSet     as IntSet
import qualified Data.SVector  as SV
import           Data.These
import           Data.Semialign (Semialign(..))

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
          consed = [ c:cs | (c, css) <- Map.toAscList nexts, cs <- css ]
      in nil ++ consed

isEmpty :: ADFA c -> Bool
isEmpty = foldNodes f
  where
    f acc nexts = not acc && and nexts

-- | Counts the number of all unique strings an ADFA accepts.
stringCount :: ADFA c -> Int
stringCount = foldNodes' f
  where
    f acc nexts = (if acc then 1 else 0) + sum nexts

-- | Returns the number of nodes.
size :: ADFA c -> Int
size (MkDFA nodes _) = length nodes

-- | Equivalence of ADFA.
equivalent :: (Ord c) => ADFA c -> ADFA c -> Bool
equivalent (prune -> MkDFA nodesA rootA) (prune -> MkDFA nodesB rootB) =
  eqv (rootA, rootB)
  where
    eqv (a,b) =
      let Node acceptsA edgesA = nodesA ! a
          Node acceptsB edgesB = nodesB ! b
      in acceptsA == acceptsB &&
         eqvEdges (Map.toAscList edgesA) (Map.toAscList edgesB)

    eqvEdges [] [] = True
    eqvEdges [] _  = False
    eqvEdges _  [] = False
    eqvEdges ((ca,a):as') ((cb,b):bs') =
      ca == cb && eqv (a,b) && eqvEdges as' bs'

step :: (Ord c) => Table s c -> c -> Key s -> Maybe (Key s)
step dfa c x = Map.lookup c $ dfa !> x

steps :: (Ord c) => Table s c -> [c] -> Key s -> Maybe (Key s)
steps dfa = loop
 where
  loop []     x = Just x
  loop (c:cs) x = step dfa c x >>= loop cs

-- | Decides if an ADFA accepts a string.
member, notMember :: (Ord c) => [c] -> ADFA c -> Bool
member cs (MkDFA nodes root) =
  case steps nodes cs root of
    Nothing -> False
    Just x  -> nodes `accepts` x
notMember cs = not . member cs

-- | Advance the initial state of given ADFA for given input string.
--
--   For @advance xs adfa@, the resulted ADFA accepts @ys@ if and only if
--   the original @adfa@ accepts @xs ++ ys@.
advance :: (Ord c) => [c] -> ADFA c -> ADFA c
advance cs (MkDFA nodes root) =
  case steps nodes cs root of
    Nothing -> empty
    Just x  -> MkDFA nodes x

-----------------------------------------------------

-- | Constructs an ADFA which accepts a string iff one or both ADFAs accept it.
union :: (Ord c) => ADFA c -> ADFA c -> ADFA c
union (MkDFA nodesA rootA) (MkDFA nodesB rootB) = instantiate rootKey stepKey
  where
    rootKey = These rootA rootB

    stepKey (This a) = This <$> nodesA ! a
    stepKey (That b) = That <$> nodesB ! b
    stepKey (These a b) =
      let Node acceptsA edgesA = nodesA ! a
          Node acceptsB edgesB = nodesB ! b
      in Node (acceptsA || acceptsB) (align edgesA edgesB)

-- | Take union of all ADFAs in a list.
unions :: (Ord c) => [ADFA c] -> ADFA c
unions = foldl' union empty

-- | Constructs an ADFA which accepts a string iff both ADFAs accept it.
intersection :: Ord c => ADFA c -> ADFA c -> ADFA c
intersection (MkDFA nodesA rootA) (MkDFA nodesB rootB) = instantiate rootKey stepKey
  where
    rootKey = (rootA, rootB)
    stepKey (a, b) =
      let Node acceptsA edgesA = nodesA ! a
          Node acceptsB edgesB = nodesB ! b
      in Node (acceptsA && acceptsB) (Map.intersectionWith (,) edgesA edgesB)

-- | @difference x y@ constructs an ADFA which accepts a string iff
--   @x@ accepts the string and @y@ do not accepts the string.
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

-- | @symdiff x y@ constructs an ADFA which accepts a string iff
--   either @x@ or @y@ accepts the string and not both do.
symdiff :: (Ord c) => ADFA c -> ADFA c -> ADFA c
symdiff (MkDFA nodesA rootA) (MkDFA nodesB rootB) = instantiate rootKey stepKey
  where
    rootKey = These rootA rootB

    xor = (/=)

    stepKey (This a) = This <$> nodesA ! a
    stepKey (That b) = That <$> nodesB ! b
    stepKey (These a b) =
      let Node acceptsA edgesA = nodesA ! a
          Node acceptsB edgesB = nodesB ! b
      in Node (acceptsA `xor` acceptsB) (align edgesA edgesB)

-- | @append x y@ constructs an ADFA which accepts a string @cs@
--   iff there exists @as@ and @bs@ such that @as ++ bs == cs@,
--   @x@ accepts @as@, and @y@ accepts @bs@.
append :: (Ord c) => ADFA c -> ADFA c -> ADFA c
append (MkDFA nodesA rootA) (MkDFA nodesB rootB) = instantiate rootKey stepKey
  where
    rootKey = (Just rootA, emptyKeys)

    node0 = Node False Map.empty

    (<|>) = (Control.Applicative.<|>)
    unionKey (mayA1, bs1) (mayA2, bs2) = (mayA1 <|> mayA2, bs1 `IntSet.union` bs2)
    fromA a = (Just a, emptyKeys)
    fromB b = (Nothing, singletonKeys b)

    rootNodeB = nodesB ! rootB

    stepKey (mayA, bSet) =
      let Node acceptsA edgesA = maybe node0 (nodesA !) mayA
          nodeBs = (nodesB !) <$> IntSet.toAscList bSet
          nodeBs' = if acceptsA then rootNodeB : nodeBs else nodeBs
          acceptsKey = any isAccepted nodeBs'
          edgesKey =
            Map.unionsWith unionKey $
              Map.map fromA edgesA :
              map (Map.map fromB . outEdges) nodeBs'
      in Node acceptsKey edgesKey

-- | Accepts all prefixes of currently accepted strings.
prefixes :: ADFA c -> ADFA c
prefixes (MkDFA nodes root) = MkDFA nodes' root
  where
    nodes' = snd <$> SV.propagate prefixes' nodes
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
      | x `IntSet.member` accum = reachable accum xs
      | otherwise            =
          let accum' = IntSet.insert x accum
              ys = Map.elems $ nodes !> x
          in reachable accum' (ys ++ xs)

    rootKey = reachable emptyKeys [root]
    stepKey xs =
      let node0 = Node False Map.empty
          merge (Node acceptsKey edgesKey) x =
            let Node acceptsX edgesX = nodes ! x
                acceptsKey' = acceptsKey || acceptsX
                edgesX' = Map.map singletonKeys edgesX
                edgesKey' = Map.unionWith IntSet.union edgesKey edgesX'
            in Node acceptsKey' edgesKey'
      in foldl' merge node0 xs

-- | Accepts all contiguous substrings of currently accepted strings.
infixes :: (Ord c) => ADFA c -> ADFA c
infixes = suffixes . prefixes

-- | Reverse every string the ADFA contains.
reverse :: (Ord c) => ADFA c -> ADFA c
reverse (MkDFA nodes root) = instantiate rootKey stepKey
  where
    rootKey = IntSet.fromList SV.keyIsInt $
      filter (\x -> isAccepted (nodes ! x)) (SV.indices nodes)
    
    reversed = reverseTable nodes
    
    stepKey xs =
      let acceptsKey = IntSet.member root xs
          edges = foldl' (Map.unionWith IntSet.union) Map.empty $
            map (reversed !) $ IntSet.toList xs
      in Node acceptsKey edges

-- Reverse the direction of each edge in the given transition table of DFA.
reverseTable :: (Ord c) => Table s c -> SV.SVector s (Map c (IntSet.IntSet (Key s)))
reverseTable nodes = reversed
  where
    reversed0 = fmap (const Map.empty) nodes
    insertEdge node (x, c) =
      Map.insertWith IntSet.union c (singletonKeys x) node
    reversed = SV.accum insertEdge reversed0
      [ (y, (x, c)) | (x, node) <- SV.toAssoc nodes
                    , (c, y) <- Map.toList (outEdges node) ]

-- | Relabel nodes to make all paths have increasing order.
--   Applying @topSort@ also eliminates unreachable nodes.
topSort :: ADFA c -> ADFA c
topSort (MkDFA nodes root) =
  unsafeRenumber root $ topologicalSort SV.keyIsInt root (nodes !) (Map.elems . outEdges)

-- | Remove empty nodes which is not accept node and only goes to
--   other empty nodes.
prune :: ADFA c -> ADFA c
prune (MkDFA nodes root) = if rootIsEmpty then empty else dfa'
  where
    table = SV.propagate pruneStep nodes

    pruneStep (Node acceptsX edgesX) =
      let edgesX' = Map.mapMaybe id edgesX
          isEmptyX = not acceptsX && Map.null edgesX'
      in if isEmptyX then Nothing else Just (Node acceptsX edgesX')

    rootIsEmpty = case table ! root of
      Nothing -> True
      Just _  -> False

    dfa' = unsafeRenumber root
      [ (x,node) | (x, Just node) <- SV.toAssoc table ]

type ReverseIndex s c = Map (Node c (Key s)) (Key s)

-- | Minimizes an ADFA by removing redundant nodes.
--   Applying @minify@ also removes unreachable nodes,
--   so there is no need to 'prune' right before or after 'minify'.
minify :: forall c. (Ord c) => ADFA c -> ADFA c
minify (MkDFA nodes root) =
  postprocess $ execState (go root) (dup0, subst0)
  where
    subst0 = IntMap.empty SV.keyIsInt
    dup0 = Map.empty

    addNode :: Key s -> Node c (Key s) -> ReverseIndex s c ->
                (Key s, ReverseIndex s c)
    addNode x node dup =
      case Map.insertLookupWithKey (\_ _ x0 -> x0) node x dup of
        (Nothing, dup') -> (x, dup')
        (Just x0, dup') -> (x0, dup')

    go x = do
      (_, subst) <- get
      case IntMap.lookup x subst of
        Nothing -> do
          let Node accepted neighbours = nodes ! x
          neighbours' <- traverse go neighbours
          let neighbours'' = Map.mapMaybe id neighbours'
              getRemoved = Map.null neighbours''  && not accepted
          if getRemoved
            then do
              modifySnd (IntMap.insert x Nothing)
              return Nothing
            else do
              x0 <- stateFst (addNode x (Node accepted neighbours''))
              modifySnd (IntMap.insert x (Just x0))
              return (Just x0)
        Just r -> return r

    postprocess (dup, subst) =
      case IntMap.lookup root subst of
        Just (Just root') ->
          let table = map swap $ Map.toList dup
          in  unsafeRenumber root' table
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

emptyKeys :: IntSet.IntSet (Key s)
emptyKeys = IntSet.empty SV.keyIsInt

singletonKeys :: Key s -> IntSet.IntSet (Key s)
singletonKeys = IntSet.singleton SV.keyIsInt

modifySnd :: (MonadState (a,b) m) => (b -> b) -> m ()
modifySnd f = get >>= \(a, b) -> let !b' = f b in put (a, b')

stateFst :: (MonadState (a,b) m) => (a -> (x,a)) -> m x
stateFst f = get >>= \(a, b) -> let (x, !a') = f a in put (a', b) >> return x
