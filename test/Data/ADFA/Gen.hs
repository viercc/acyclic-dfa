{-# LANGUAGE ViewPatterns #-}
module Data.ADFA.Gen(
  C(..),
  ADFA'(..),
  genADFA,
  acceptStrs
) where

import           Test.QuickCheck

import           Data.List          (foldl')
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           Common
import           Data.ADFA
import           Data.ADFA.Internal
import qualified Data.ADFA.IdVector as IV

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

toTable :: ADFA c -> (Int, Map Int (Node c Int))
toTable dfa = withInternals dfa idToInt
  where
    idToInt root table =
      let subst = Map.fromList $ zip (IV.indices table) [0..]
          f = (subst Map.!)
          table' = Map.fromList $ zip [0..] (IV.toList table)
          table'' = Map.map (fmap f) table'
      in (f root, table'')

shrinkADFA :: (Ord c, Arbitrary c) => ADFA c -> [ADFA c]
shrinkADFA (toTable -> (root, nodes)) =
  [ dfa'
    | n <- Map.keys nodes
    , n /= root
    , nodes' <- [deleteNode n nodes, contractNode n nodes]
    , Just dfa' <- [fromTable root (Map.toList nodes')]
    ] ++
  [ dfa'
    | (n, Node t nexts) <- Map.toList nodes
    , c <- Map.keys nexts
    , let node' = Node t (Map.delete c nexts)
          nodes' = Map.insert n node' nodes
    , Just dfa' <- [fromTable root (Map.toList nodes')]
    ]

deleteNode :: (Ord k, Ord c) => k -> Map k (Node c k) -> Map k (Node c k)
deleteNode x = Map.map removeEdge . Map.delete x
  where
    removeEdge (Node t e) = Node t (Map.filter (/= x) e)

contractNode :: (Ord k, Ord c) => k -> Map k (Node c k) -> Map k (Node c k)
contractNode x nodes =
  let node = nodes Map.! x
      nextsList = Map.toList $ outEdges node
      replaceEdges (Node t nexts) =
        let nexts' = Map.fromList $
              do (c, y) <- Map.toList nexts
                 if y == x then nextsList else [(c,y)]
        in Node t nexts'
  in Map.map replaceEdges . Map.delete x $ nodes

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
      Just result <- return $ fromTable 0 (Map.toList nodeData)
      return result
    
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
