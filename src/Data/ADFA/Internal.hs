{-| Internals for Data.ADFA -}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
module Data.ADFA.Internal(
  ADFA(..),
  Node(..),
  Table,
  empty, string, strings,
  instantiate,
  treeInstantiate,
  fromTable,

  -- * low-level running
  withInternals,

  -- * Debug
  debugShow, debugPrint,

  -- * Internals
  Key(),
  (!), (!>), accepts,
  foldNodes,
  foldNodes',

  -- * Unsafe construction
  unsafeRenumber, unsafeRenumberOrd,
) where

import           Data.Foldable             (foldl', for_)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import qualified Data.Map.Lazy             as LMap
import           Data.Maybe                (fromMaybe)

import qualified Data.Set                  as Set

import Control.Applicative hiding (empty)
import qualified Control.Category(id)
import Data.Type.Coercion.Sub hiding (instantiate)

import qualified Data.Vector               as V
--import qualified Data.MSeq                 as MS
import qualified Data.Vector.Growable as GV

import qualified Newtype.IntMap            as IM
import Data.SVector as SV
import qualified Data.SVector.Internal as Unsafe

import           Control.DeepSeq

import           Data.Functor.Classes
import Control.Monad.ST
import Data.STRef

refl :: Sub a a
refl = Control.Category.id

-- | Acyclic DFA.
data ADFA c where
  MkDFA ::
    {
      getNodes :: !(Table s c),
      rootNode :: !(Key s)
    } -> ADFA c

type Table s c = SVector s (Node c (Key s))

deriving instance Show c => Show (ADFA c)

instance Eq c => Eq (ADFA c) where
  MkDFA nodesA rootA == MkDFA nodesB rootB =
    liftEq (liftEq eqKey) (getRawVector nodesA) (getRawVector nodesB) &&
    getKey rootA == getKey rootB
    where
      eqKey = upcastWith (keyIsInt `arrR` keyIsInt `arrR` refl) (==)

instance Ord c => Ord (ADFA c) where
  MkDFA nodesA rootA `compare` MkDFA nodesB rootB =
    liftCompare (liftCompare compareKey) (getRawVector nodesA) (getRawVector nodesB) <>
    rootA `compareKey` rootB
    where
      compareKey = upcastWith (keyIsInt `arrR` keyIsInt `arrR` refl) compare

instance NFData c => NFData (ADFA c) where
  rnf (MkDFA nodes _) = rnf nodes

-- | A Node of ADFA.
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

-- * Smart ADFA Constructors

makeDFA :: V.Vector (Node c Int) -> Int -> ADFA c
makeDFA nodes root = fromRawVector nodes $
  \nodes' check ->
     let check' i = maybe (Left i) Right $ check i
         nodes'' = traverse (traverse check') nodes'
     in case liftA2 (,) nodes'' (check' root) of
          Left i -> error $ "Out-of-bounds:" ++ show i
          Right (checkedNodes, checkedRoot) -> MkDFA checkedNodes checkedRoot

unsafeMakeDFA :: V.Vector (Node c Int) -> Int -> ADFA c
unsafeMakeDFA nodes root = MkDFA (Unsafe.SV $ fmap (fmap Unsafe.Key) nodes) (Unsafe.Key root)

-- | Empty ADFA which accepts no string.
empty :: ADFA c
empty = makeDFA (V.singleton (Node False Map.empty)) 0

-- | Construct from a string.
string :: [c] -> ADFA c
string cs = makeDFA nodes 0
  where makeNode i c = Node False (Map.singleton c (i+1))
        nodes = V.fromList $ zipWith makeNode [0..] cs ++ [Node True Map.empty]

-- | Construct from strings. Resulted ADFA has tree-like structure,
--   which might not be the most compact representation.
strings :: forall c. (Ord c) => [[c]] -> ADFA c
strings css = makeDFA nodes 0
  where
    nodes = V.create $ do
      vnodes <- GV.new 1
      GV.unsafeWrite vnodes 0 (Node False Map.empty)
      _ <- insertAll vnodes 1 css
      GV.unsafeToMVector vnodes

    insertAll :: GV.GrowVector s (Node c Int) -> Int -> [[c]] -> ST s Int
    insertAll _vnodes len [] = return len
    insertAll vnodes len (word:rest) = loop 0 word >>= \adds -> insertAll vnodes (len + adds) rest
      where
        loop x [] = do
          GV.modify vnodes (\node -> node{isAccepted = True}) x
          return 0
        loop x (c:cs) = do
          Node accX edgesX <- GV.read vnodes x
          case Map.lookup c edgesX of
            Just y -> loop y cs
            Nothing -> do
              let node' = Node accX $ Map.insert c len edgesX
              GV.write vnodes x node'
              let restLen = length cs
                  links = zip cs [1 .. restLen]
                  newNodes = [ Node False (Map.singleton a (len + i)) | (a,i) <- links ] ++ [ Node True Map.empty ]
              GV.grow (restLen + 1) vnodes
              for_ (zip [len ..] newNodes) $ uncurry (GV.write vnodes)
              return (length (c:cs))

instantiate :: (Ord k) => k -> (k -> Node c k) -> ADFA c
instantiate rootKey stepKey = makeDFA nodes 0
  where
    nodes = V.create $ do
      vnodes <- GV.new 0
      substRef <- newSTRef Map.empty
      let assign key = do
            subst <- readSTRef substRef
            case Map.lookup key subst of
              Just x  -> return x
              Nothing -> newNode key
          newNode key = do
            subst <- readSTRef substRef
            GV.grow 1 vnodes
            let x = Map.size subst
            writeSTRef substRef $! Map.insert key x subst
            nodeX <- traverse assign (stepKey key)
            GV.write vnodes x nodeX
            return x
      _ <- assign rootKey
      GV.unsafeToMVector vnodes

treeInstantiate :: k -> (k -> Node c k) -> ADFA c
treeInstantiate rootKey stepKey = makeDFA nodes 0
  where
    nodes = V.create $ do
      vnodes <- GV.new 0
      let assign key = do
            x <- GV.length vnodes
            GV.grow 1 vnodes
            nodeX <- traverse assign (stepKey key)
            GV.write vnodes x nodeX
            return x
      _ <- assign rootKey
      GV.unsafeToMVector vnodes

fromTable :: Ord k => k -> [(k, Node c k)] -> Maybe (ADFA c)
fromTable root nodes =
  if noUndefinedKeys && isAcyclic nodeGraph
    then Just $ unsafeRenumberOrd root nodes
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
              f acc j =
                let descendants = fromMaybe Set.empty $ Map.lookup j reach'
                in acc `Set.union` descendants
              rs0 = Set.fromList children
              rs = foldl' f rs0 children
          in Map.insert k rs reach'

-- | Consume internal structures
withInternals :: ADFA c -> (forall s. Key s -> Table s c -> r) -> r
withInternals (MkDFA nodes root) f = f root nodes

-- * Debug
debugPrint :: (Show c) => ADFA c -> IO ()
debugPrint = putStrLn . debugShow

debugShow :: (Show c) => ADFA c -> String
debugShow (MkDFA nodes root) =
  unlines $ concatMap oneNode (toAssoc nodes)
  where
    showNodePretty x t =
      let rootIndicator = if x == root then "r" else " "
          typeIndicator = if t then "a" else " "
      in show x ++ rootIndicator ++ typeIndicator
    oneNode (x, Node t nexts) = showNodePretty x t : map ((" " ++) . oneEdge) (Map.toAscList nexts)
    oneEdge (c, x') = show c ++ "->" ++ show x'

-- * Utility

(!>) :: Table s c -> Key s -> Map c (Key s)
(!>) nodes x = outEdges (nodes ! x)

accepts :: Table s c -> Key s -> Bool
accepts dfa x = isAccepted $ dfa ! x

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
foldNodes' f (MkDFA nodes root) = table ! root
  where table = fmap g nodes
        g (Node t e) = f t (LMap.map (table !) e)

-- * Unsafe operations
unsafeRenumber :: Key s -> [(Key s, Node c (Key s))] -> ADFA c
unsafeRenumber rootX sortedNodes = unsafeMakeDFA nodes rootY
  where
    xs = map fst sortedNodes
    subst = IM.fromList keyIsInt $ zip xs [0..]
    n = IM.size subst

    applySubst x = case IM.lookup x subst of
      Nothing -> error "renumber: Never reach here"
      Just y  -> y
    rootY = applySubst rootX
    nodesY = map (fmap applySubst . snd) sortedNodes
    nodes = V.fromListN n nodesY

unsafeRenumberOrd :: (Ord k) => k -> [(k, Node c k)] -> ADFA c
unsafeRenumberOrd rootX sortedNodes = unsafeMakeDFA nodes rootY
  where
    xs = map fst sortedNodes
    subst = Map.fromList $ zip xs [0..]
    n = Map.size subst

    applySubst = fmap (subst Map.!)
    rootY = subst Map.! rootX
    nodes = V.fromListN n $ map (applySubst . snd) sortedNodes

