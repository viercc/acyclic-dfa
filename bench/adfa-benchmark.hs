{-# OPTIONS_GHC -fno-full-laziness #-}
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

shuffleList :: R.GenIO -> [a] -> IO [a]
shuffleList g xs = V.toList <$> R.uniformShuffle (V.fromList xs) g

dictAmEnFile, dictBrEnFile, uri1File, uri2File :: FilePath
dictAmEnFile = "/usr/share/dict/american-english"
dictBrEnFile = "/usr/share/dict/british-english"
uri1File = "benchdata/externallinks.txt.1"
uri2File = "benchdata/externallinks.txt.2"

dictDataSet :: IO ([String], [String], [String], [String])
dictDataSet =
  do g <- R.initialize (seed 1)
     dictASorted <- sort . lines <$> readFile dictAmEnFile
     dictAShuf <- shuffleList g dictASorted
     dictB <- lines <$> readFile dictBrEnFile
     let small = take 1000 dictAShuf
     return (dictASorted, dictAShuf, dictB, small)

uriDataSet :: IO ([String], [String], [String], [String])
uriDataSet =
  do g <- R.initialize (seed 1)
     dictASorted <- sort . lines <$> readFile uri1File 
     dictAShuf <- shuffleList g dictASorted
     dictB <- lines <$> readFile uri2File
     let small = take 1000 dictAShuf
     return (dictASorted, dictAShuf, dictB, small)

randomStrs :: IO [String]
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
main = defaultMain
  [ env dictDataSet $ \dataSet ->
      bgroup "dict" [ benchADFA dataSet, benchTrie dataSet, benchSet dataSet ]
  , env uriDataSet $ \dataSet ->
      bgroup "URI" [ benchADFA dataSet, benchTrie dataSet, benchSet dataSet ]
  ]

benchADFA :: ([String], [String], [String], [String]) -> Benchmark
benchADFA ~(dictASorted, dictAShuf, dictB, small) =
  env mkDFAs $ \ ~(dfaA, dfaA', _, dfaB', dfaSmall) ->
  bgroup "ADFA"
  [ bgroup "construction"
    [ bench "fromList" $ whnf ADFA.fromList dictAShuf
    , bench "fromAscList" $ whnf ADFA.fromAscList dictASorted ]
  , bgroup "query"
      [ bench "isEmpty" (whnf ADFA.isEmpty dfaA')
      , bench "stringCount" (whnf ADFA.stringCount dfaA')
      , bench "enumerate10" (whnf (take 10 . ADFA.enumerate) dfaA')
      , env randomStrs $ \qs ->
          bench "member" (nf (\dfa' -> map (`ADFA.member` dfa') qs) dfaA')
      , bench "eqv1" (whnf (ADFA.equivalent dfaA') dfaA')
      , bench "eqv2" (whnf (ADFA.equivalent dfaA') dfaA)
      , bench "eqv3" (whnf (ADFA.equivalent dfaA') dfaB') ]
  , bgroup "optimize"
      [ bench "minify" (whnf ADFA.minify dfaA)
      , bench "topSort" (whnf ADFA.topSort dfaA)
      , bench "topSort'" (whnf ADFA.topSort dfaA')
      , bench "prune" (whnf ADFA.prune dfaA)
      , bench "prune'" (whnf ADFA.prune dfaA') ]
  , bgroup "combine"
      [ bench "union" (whnf (ADFA.union dfaA') dfaB')
      , bench "intersection" (whnf (ADFA.intersection dfaA') dfaB')
      , bench "difference" (whnf (ADFA.difference dfaA') dfaB')
      , bench "symdiff" (whnf (ADFA.symdiff dfaA') dfaB')
      , bench "append" (whnf (ADFA.append dfaSmall) dfaSmall)
      , bench "prefixes" (whnf ADFA.prefixes dfaA')
      , bench "suffixes" (whnf ADFA.suffixes dfaB')
      , bench "reverse" (whnf ADFA.reverse dfaA')
      , bench "reverse_manual" (whnf manualReverse dfaA')]
  ]
  where
    mkDFAs =
      let dfaA = ADFA.fromAscList dictASorted
          dfaA' = ADFA.minify dfaA
          dfaB = ADFA.fromList dictB
          dfaB' = ADFA.minify dfaB
          dfaSmall = ADFA.minify $ ADFA.fromList small
      in return (dfaA, dfaA', dfaB, dfaB', dfaSmall)

manualReverse :: (Ord c) => ADFA.ADFA c -> ADFA.ADFA c
manualReverse = ADFA.fromList . map reverse . ADFA.toList

benchTrie ::  ([String], [String], [String], [String]) -> Benchmark
benchTrie ~(dictASorted, dictAShuf, dictB, small) =
  env mkTries $ \ ~(a, b, c) ->
  bgroup "Trie"
  [ bgroup "construction"
    [ bench "fromList" $ whnf T.fromList dictAShuf
    , bench "fromAscList" $ whnf T.fromAscList dictASorted ]
  , bgroup "query"
      [ bench "isEmpty" (whnf T.null a)
      , bench "stringCount" (whnf T.count a)
      , bench "enumerate10" (whnf (take 10 . T.toList) a)
      , env randomStrs $ \qs ->
          bench "member" (nf (\dfa' -> map (`T.member` dfa') qs) a)
      , bench "eqv1" (whnf (a ==) a)
      , bench "eqv3" (whnf (a ==) b) ]
  , bgroup "combine"
      [ bench "union" (whnf (T.union a) b)
      , bench "intersection" (whnf (T.intersection a) b)
      , bench "difference" (whnf (T.difference a) b)
      , bench "append" (whnf (T.append c) c)
      , bench "prefixes" (nf T.prefixes a)
      , bench "suffixes" (whnf T.suffixes b)
      , bench "reverse" (whnf trieReverse a)
      ]
  ]
  where
    mkTries = return ( T.fromAscList dictASorted
                     , T.fromList dictB
                     , T.fromList small )


trieReverse :: (Ord c) => T.TSet c -> T.TSet c
trieReverse = T.fromList . map reverse . T.toList

benchSet :: ([String], [String], [String], [String]) -> Benchmark
benchSet ~(dictASorted, dictAShuf, dictB, small) =
  env mkSets $ \ ~(a, b, c) ->
  bgroup "Set"
  [ bgroup "construction"
    [ bench "fromList" $ whnf Set.fromList dictAShuf
    , bench "fromAscList" $ whnf Set.fromAscList dictASorted ]
  , bgroup "query"
      [ bench "isEmpty" (whnf Set.null a)
      , bench "stringCount" (whnf Set.size a)
      , bench "enumerate10" (whnf (take 10 . Set.toList) a)
      , env randomStrs $ \qs ->
          bench "member" (nf (\dfa' -> map (`Set.member` dfa') qs) a)
      , bench "eqv1" (whnf (a ==) a)
      , bench "eqv3" (whnf (a ==) b) ]
  , bgroup "combine"
      [ bench "union" (whnf (Set.union a) b)
      , bench "intersection" (whnf (Set.intersection a) b)
      , bench "difference" (whnf (Set.difference a) b)
      , bench "append" (whnf (setAppend c) c)
      , bench "prefixes" (whnf setPrefixes a)
      , bench "suffixes" (whnf setSuffixes b)
      , bench "reverse" (whnf setReverse a)
      ]
  ]
  where
    mkSets = return ( Set.fromAscList dictASorted
                    , Set.fromList dictB
                    , Set.fromList small )

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
