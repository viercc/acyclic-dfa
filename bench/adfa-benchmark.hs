module Main(main) where

import Criterion.Main

import qualified Data.ADFA           as ADFA

import qualified Data.Trie.Set as T

import Data.List (sort, inits, tails)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Word

import qualified System.Random.MWC                as R
import qualified System.Random.MWC.CondensedTable as R
import qualified System.Random.MWC.Distributions  as R

numRandomStr :: Int
numRandomStr = 1000

seed :: Word32 -> V.Vector Word32
seed w = V.fromList [1573289798, 32614861, w]

dictAmEn, dictBrEn, dictAmEnShuffled, randomStrs :: IO [String]
dictAmEn = lines <$> readFile "/usr/share/dict/american-english"
dictBrEn = lines <$> readFile "/usr/share/dict/british-english"
dictAmEnShuffled =
  do g <- R.initialize (seed 1)
     ws <- V.fromList <$> dictAmEn
     V.toList <$> R.uniformShuffle ws g
randomStrs =
  do g <- R.initialize (seed 3)
     revReplicateM numRandomStr $ do
       n <- R.genFromTable distN g
       revReplicateM (n+1) (uniformAlphabet g)
  where
    distN = R.tableBinomial 12 0.33
    alphabet = V.fromList ['a' .. 'z']
    numAlphabet = V.length alphabet
    uniformAlphabet g = (alphabet V.!) <$> R.uniformR (0, numAlphabet-1) g

main :: IO ()
main = defaultMain [ benchADFA, benchTrie, benchSet ]

benchADFA :: Benchmark
benchADFA = bgroup "ADFA" 
  [ bgroup "construction"
    [ env dictAmEnShuffled $ \dict -> bench "fromList" $ nf ADFA.fromList dict
    , env (sort <$> dictAmEn) $ \sortedDict ->
        bench "fromAscList" $ nf ADFA.fromAscList sortedDict ]
  , env (ADFA.minify . ADFA.fromList <$> dictAmEn) $ \dfa ->
      bgroup "query"
        [ bench "isEmpty" (nf ADFA.isEmpty dfa)
        , bench "stringCount" (nf ADFA.stringCount dfa)
        , bench "enumerate10" (nf (take 10 . ADFA.enumerate) dfa)
        , env randomStrs $ \qs ->
            bench "member" (nf (\dfa' -> map (`ADFA.member` dfa') qs) dfa)
        , bench "eqv1" (nf (ADFA.equivalent dfa) dfa)
        , env (ADFA.minify . ADFA.fromList <$> dictBrEn) $ \dfa' ->
            bench "eqv2" (nf (ADFA.equivalent dfa) dfa') ]
  , env (ADFA.fromList <$> dictAmEn) $ \dfa ->
    env (return $ ADFA.minify dfa) $ \dfa' ->
      bgroup "optimize"
        [ bench "minify" (nf ADFA.minify dfa)
        , bench "topSort" (nf ADFA.topSort dfa')
        , bench "prune" (nf ADFA.prune dfa') ]
  , env (ADFA.minify . ADFA.fromList <$> dictAmEn) $ \dfaA ->
    env (ADFA.minify . ADFA.fromList <$> dictBrEn) $ \dfaB ->
    env (ADFA.minify . ADFA.fromList <$> randomStrs) $ \dfaSmall ->
      bgroup "combine"
        [ bench "union" (nf (uncurry ADFA.union) (dfaA, dfaB))
        , bench "intersection" (nf (uncurry ADFA.intersection) (dfaA, dfaB))
        , bench "difference" (nf (uncurry ADFA.difference) (dfaA, dfaB))
        , bench "append" (nf (uncurry ADFA.append) (dfaSmall, dfaSmall))
        , bench "prefixes" (nf ADFA.prefixes dfaA)
        , bench "suffixes" (nf ADFA.suffixes dfaB) ]
  ]

benchTrie :: Benchmark
benchTrie = bgroup "Trie" 
  [ bgroup "construction"
      [ env dictAmEnShuffled $ \dict -> bench "fromList" $ nf T.fromList dict
      , env (sort <$> dictAmEn) $ \sortedDict ->
          bench "fromAscList" $ nf T.fromAscList sortedDict ]
  , env (T.fromList <$> dictAmEn) $ \dict ->
      bgroup "query"
        [ bench "isEmpty" (nf T.null dict)
        , bench "stringCount" (nf T.count dict)
        , bench "enumerate10" (nf (take 10 . T.enumerate) dict)
        , env randomStrs $ \qs ->
            bench "match" (nf (\dict' -> map (`T.member` dict') qs) dict)
        , bench "eqv1" (nf (dict ==) dict)
        , env (T.fromList <$> dictBrEn) $ \dict' ->
            bench "eqv2" (nf (dict ==) dict')]
  , env (T.fromList <$> dictAmEn) $ \dictA ->
    env (T.fromList <$> dictBrEn) $ \dictB ->
    env (T.fromList <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (nf (uncurry T.union) (dictA, dictB))
        , bench "intersection" (nf (uncurry T.intersection) (dictA, dictB))
        , bench "difference" (nf (uncurry T.difference) (dictA, dictB))
        , bench "append" (nf (uncurry T.append) (dictSmall, dictSmall))
        , bench "prefixes" (nf T.prefixes dictA)
        , bench "suffixes" (nf T.suffixes dictB) ]
  ]
    
benchSet :: Benchmark
benchSet = bgroup "Set" 
  [ bgroup "construction"
      -- Set.fromList detects whether the input list is sorted
      -- and switch the algorithm based on it.
      -- Using shuffled dictionary avoids this optimization fires
      -- in this benchmark.
      [ env dictAmEnShuffled $ \dict -> bench "fromList" $ nf Set.fromList dict
      , env (sort <$> dictAmEn) $ \sortedDict ->
          bench "fromAscList" $ nf Set.fromAscList sortedDict ]
  , env (Set.fromList <$> dictAmEn) $ \dictSet ->
      bgroup "query"
        [ bench "isEmpty" (nf Set.null dictSet)
        , bench "stringCount" (nf Set.size dictSet)
        , bench "enumerate10" (nf (take 10 . Set.toList) dictSet)
        , env randomStrs $ \qs ->
            bench "match" (nf (\dictSet' -> map (`Set.member` dictSet') qs) dictSet)
        , bench "eqv1" (nf (dictSet ==) dictSet)
        , env (Set.fromList <$> dictBrEn) $ \dictSet' ->
            bench "eqv2" (nf (dictSet ==) dictSet')]
  , env (Set.fromList <$> dictAmEn) $ \dictA ->
    env (Set.fromList <$> dictBrEn) $ \dictB ->
    env (Set.fromList <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (nf (uncurry Set.union) (dictA, dictB))
        , bench "intersection" (nf (uncurry Set.intersection) (dictA, dictB))
        , bench "difference" (nf (uncurry Set.difference) (dictA, dictB))
        , bench "append" (nf (uncurry setAppend) (dictSmall, dictSmall))
        , bench "prefixes" (nf setPrefixes dictA)
        , bench "suffixes" (nf setSuffixes dictB) ]
  ]

setAppend :: (Ord c) => Set [c] -> Set [c] -> Set [c]
setAppend ass bss = Set.unions
  [ Set.mapMonotonic (as ++) bss
      | as <- Set.toAscList ass ]

setPrefixes :: (Ord c) => Set [c] -> Set [c]
setPrefixes ass = Set.unions
  [ Set.fromDistinctAscList (inits as) | as <- Set.toAscList ass ]

setSuffixes :: (Ord c) => Set [c] -> Set [c]
setSuffixes ass = Set.fromList
  [ bs | as <- Set.toAscList ass, bs <- tails as ]

-------------------------------------------------------------------
-- Utility

revReplicateM :: (Monad m) => Int -> m a -> m [a]
revReplicateM n ma = loop n []
  where
    loop 0 acc = return acc
    loop i acc = ma >>= \a -> loop (i-1) (a:acc)
