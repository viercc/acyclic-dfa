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
        [ bench "isEmpty" (whnf ADFA.isEmpty dfa)
        , bench "stringCount" (whnf ADFA.stringCount dfa)
        , bench "enumerate10" (whnf (take 10 . ADFA.enumerate) dfa)
        , env randomStrs $ \qs ->
            bench "member" (whnf (\dfa' -> map (`ADFA.member` dfa') qs) dfa)
        , bench "eqv1" (whnf (ADFA.equivalent dfa) dfa)
        , env (ADFA.minify . ADFA.fromList <$> dictBrEn) $ \dfa' ->
            bench "eqv2" (whnf (ADFA.equivalent dfa) dfa') ]
  , env (ADFA.fromList <$> dictAmEn) $ \dfa ->
    env (return $ ADFA.minify dfa) $ \dfa' ->
      bgroup "optimize"
        [ bench "minify" (whnf ADFA.minify dfa)
        , bench "topSort" (whnf ADFA.topSort dfa')
        , bench "prune" (whnf ADFA.prune dfa') ]
  , env (ADFA.minify . ADFA.fromList <$> dictAmEn) $ \dfaA ->
    env (ADFA.minify . ADFA.fromList <$> dictBrEn) $ \dfaB ->
    env (ADFA.minify . ADFA.fromList <$> randomStrs) $ \dfaSmall ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry ADFA.union) (dfaA, dfaB))
        , bench "intersection" (whnf (uncurry ADFA.intersection) (dfaA, dfaB))
        , bench "difference" (whnf (uncurry ADFA.difference) (dfaA, dfaB))
        , bench "append" (whnf (uncurry ADFA.append) (dfaSmall, dfaSmall))
        , bench "prefixes" (whnf ADFA.prefixes dfaA)
        , bench "suffixes" (whnf ADFA.suffixes dfaB)
        , bench "reverse" (whnf ADFA.reverse dfaA)
        , bench "reverse_manual" (whnf manualReverse dfaA)]
  ]

manualReverse :: (Ord c) => ADFA.ADFA c -> ADFA.ADFA c
manualReverse = ADFA.fromList . map reverse . ADFA.toList

benchTrie :: Benchmark
benchTrie = bgroup "Trie" 
  [ bgroup "construction"
      [ env dictAmEnShuffled $ \dict -> bench "fromList" $ nf T.fromList dict
      , env (sort <$> dictAmEn) $ \sortedDict ->
          bench "fromAscList" $ nf T.fromAscList sortedDict ]
  , env (T.fromList <$> dictAmEn) $ \dict ->
      bgroup "query"
        [ bench "isEmpty" (whnf T.null dict)
        , bench "stringCount" (whnf T.count dict)
        , bench "enumerate10" (whnf (take 10 . T.enumerate) dict)
        , env randomStrs $ \qs ->
            bench "match" (whnf (\dict' -> map (`T.member` dict') qs) dict)
        , bench "eqv1" (whnf (dict ==) dict)
        , env (T.fromList <$> dictBrEn) $ \dict' ->
            bench "eqv2" (whnf (dict ==) dict')]
  , env (T.fromList <$> dictAmEn) $ \dictA ->
    env (T.fromList <$> dictBrEn) $ \dictB ->
    env (T.fromList <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry T.union) (dictA, dictB))
        , bench "intersection" (whnf (uncurry T.intersection) (dictA, dictB))
        , bench "difference" (whnf (uncurry T.difference) (dictA, dictB))
        , bench "append" (whnf (uncurry T.append) (dictSmall, dictSmall))
        , bench "prefixes" (whnf T.prefixes dictA)
        , bench "suffixes" (whnf T.suffixes dictB)
        , bench "reverse" (whnf trieReverse dictA)]
  ]

trieReverse :: (Ord c) => T.TSet c -> T.TSet c
trieReverse = T.fromList . map reverse . T.toList

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
        [ bench "isEmpty" (whnf Set.null dictSet)
        , bench "stringCount" (whnf Set.size dictSet)
        , bench "enumerate10" (whnf (take 10 . Set.toList) dictSet)
        , env randomStrs $ \qs ->
            bench "match" (whnf (\dictSet' -> map (`Set.member` dictSet') qs) dictSet)
        , bench "eqv1" (whnf (dictSet ==) dictSet)
        , env (Set.fromList <$> dictBrEn) $ \dictSet' ->
            bench "eqv2" (whnf (dictSet ==) dictSet')]
  , env (Set.fromList <$> dictAmEn) $ \dictA ->
    env (Set.fromList <$> dictBrEn) $ \dictB ->
    env (Set.fromList <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry Set.union) (dictA, dictB))
        , bench "intersection" (whnf (uncurry Set.intersection) (dictA, dictB))
        , bench "difference" (whnf (uncurry Set.difference) (dictA, dictB))
        , bench "append" (whnf (uncurry setAppend) (dictSmall, dictSmall))
        , bench "prefixes" (whnf setPrefixes dictA)
        , bench "suffixes" (whnf setSuffixes dictB)
        , bench "reverse" (whnf setReverse dictA) ]
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

setReverse :: (Ord c) => Set [c] -> Set [c]
setReverse = Set.map reverse

-------------------------------------------------------------------
-- Utility

revReplicateM :: (Monad m) => Int -> m a -> m [a]
revReplicateM n ma = loop n []
  where
    loop 0 acc = return acc
    loop i acc = ma >>= \a -> loop (i-1) (a:acc)
