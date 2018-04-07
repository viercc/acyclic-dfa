module Data.ADFA.Gen(
  C(..),
  ADFA'(..),
  genADFA,
  acceptStrs
) where

import           Test.QuickCheck

import           Data.List          (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import qualified Data.Vector     as V

import           Common
import           Data.ADFA.Internal
import           Data.ADFA.Algorithm

newtype ADFA' = ADFA' (ADFA C)

instance Show ADFA' where
  show (ADFA' dfa) = debugShow dfa

instance Arbitrary ADFA' where
  arbitrary = ADFA' <$> genADFA
  shrink (ADFA' dfa) = ADFA' <$> shrinkADFA dfa

genADFA :: (Ord c, Arbitrary c) => Gen (ADFA c)
genADFA = frequency
  [ (4, handgenADFA)
  , (2, strings <$> arbitrary)
  ]

shrinkADFA :: (Ord c, Arbitrary c) => ADFA c -> [ADFA c]
shrinkADFA (MkDFA nodes root) =
  [ MkDFA nodes' root
    | n <- NodeId <$> [0 .. V.length nodes - 1]
    , n /= root
    , nodes' <- [deleteNode n nodes, contractNode n nodes]
    ] ++
  [ MkDFA nodes' root
    | (n, Node t nexts) <- vecToListI nodes
    , c <- Map.keys nexts
    , let node' = Node t (Map.delete c nexts)
          nodes' = nodes V.// [(n, node')]
    ] ++
  [ dfa'
    | let dfa' = topSort (MkDFA nodes root)
    , V.length (getNodes dfa') < V.length nodes]

vecToListI :: V.Vector a -> [(Int, a)]
vecToListI = V.ifoldr (\i a r -> (i, a) : r) []

deleteNode :: (Ord c) => NodeId -> V.Vector (Node c NodeId) -> V.Vector (Node c NodeId)
deleteNode n = V.map removeEdge
  where
    removeEdge (Node t e) = Node t (Map.filter (/= n) e)

contractNode :: (Ord c) => NodeId -> V.Vector (Node c NodeId) -> V.Vector (Node c NodeId)
contractNode n nodes =
  let node = nodes V.! getNodeId n
      nextsList = Map.toList $ outEdges node
      replaceEdges (Node t nexts) =
        let nexts' = Map.fromList $
              do (c, x) <- Map.toList nexts
                 if x == n then nextsList else [(c,x)]
        in Node t nexts'
  in V.map replaceEdges nodes

handgenADFA :: (Ord c, Arbitrary c) => Gen (ADFA c)
handgenADFA =
    sized $ \n -> handgen n `suchThat` reasonablySizedADFA n
  where
    handgen n = do
      nodeNum <- choose (2, max 2 n)
      let lastN = nodeNum-1
          ns = [0..lastN]
      goals <- Set.insert lastN . Set.fromList <$> vectorOf (n `div` 4) (elements ns)
      edges <- scale (*3) $ listOf $
        do (x,x') <- randomPair ns
           c <- resize n arbitrary
           return (x, c, x')
      let nodeData0 = Map.fromList [ (x, Node (Set.member x goals) Map.empty) | x <- ns ]
          nodeData = foldl' addEdge nodeData0 edges
      return $ renumber 0 (Map.toList nodeData)
    
    addEdge nodes (x, c, x') =
      let f (Node t e) = Node t (Map.insert c x' e)
      in Map.adjust f x nodes

reasonablySizedADFA :: Int -> ADFA c -> Bool
reasonablySizedADFA n dfa =
  let sc = stringCount dfa
      lo = sqrti n
      hi = n * sqrti n
  in lo <= sc && sc <= hi

acceptStrs :: ADFA c -> Gen [[c]]
acceptStrs dfa = sized $ \n ->
  let m = stringCount dfa
      loop _ [] = return []
      loop k (a:as)
        | k <= 0    = return []
        | otherwise =
            frequency [(n, (a:) <$> loop (k-1) as), (m, loop k as)]
  in loop n (enumerate dfa)
