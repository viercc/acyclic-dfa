module Data.ADFA.ADFASpec(
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.List (inits, tails)
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           Data.ADFA.Internal  as ADFA
import           Data.ADFA.Algorithm as ADFA

import           Data.ADFA.Gen

spec :: Spec
spec = do
  specify "enumerate empty = []" $
    enumerate (empty :: ADFA C) `shouldSatisfy` null
  specify "enumerate . string = (:[])" $
    property $ \str -> enumerate (string (str :: [C])) === [str]
  specify "enumerate . strings = Set.toAscList . Set.fromList" $
    property $ \strs -> (enumerate . strings) (strs :: [[C]]) == (Set.toAscList . Set.fromList) strs
  specify "null . enumerate = isEmpty" $
    property $ \(ADFA' dfa) -> null (enumerate dfa) === isEmpty dfa
  specify "length . enumerate = stringCount" $
    property $ \(ADFA' dfa) -> length (enumerate dfa) === stringCount dfa
  
  specify "match dfa = (`Set.member` toSet dfa)" $
    property $ \(ADFA' dfa) ->
      let strSet = toSet dfa
      in property $ \str -> match dfa str == Set.member str strSet
  specify "forAll (str `in` enumerate dfa). match dfa str" $
    property $ \(ADFA' dfa) -> all (match dfa) <$> acceptStrs dfa
  
  specify "equivalent a b == (enumerate a == enumerate b)" $
    property $ \(ADFA' dfaA) (ADFA' dfaB) ->
      equivalent dfaA dfaB === (enumerate dfaA == enumerate dfaB)
  
  specify "minify a `equivalent` a" $
    property $ \(ADFA' dfa) -> minify dfa `equivalent` dfa
  specify "topSort a `equivalent` a" $
    property $ \(ADFA' dfa) -> topSort dfa `equivalent` dfa
  specify "prune a `equivalent` a" $
    property $ \(ADFA' dfa) -> prune dfa `equivalent` dfa
    
  specify "toSet (union a b) = Set.union (toSet a) (toSet b)" $
    property $ \(ADFA' dfaA) (ADFA' dfaB) ->
      toSet (union dfaA dfaB) == Set.union (toSet dfaA) (toSet dfaB)
  specify "toSet (intersection a b) = Set.intersection (toSet a) (toSet b)" $
    property $ \(ADFA' dfaA) (ADFA' dfaB) ->
      toSet (intersection dfaA dfaB) == Set.intersection (toSet dfaA) (toSet dfaB)
  specify "toSet (difference a b) = Set.difference (toSet a) (toSet b)" $
    property $ \(ADFA' dfaA) (ADFA' dfaB) ->
      toSet (difference dfaA dfaB) == Set.difference (toSet dfaA) (toSet dfaB)
  specify "toSet (append a b) = setAppend (toSet a) (toSet b)" $
    property $ \(ADFA' dfaA) (ADFA' dfaB) ->
      toSet (append dfaA dfaB) == setAppend (toSet dfaA) (toSet dfaB)
  
  
  specify "toSet (prefixes a) = setPrefixes (toSet a)" $
    property $ \(ADFA' dfa) -> toSet (prefixes dfa) === setPrefixes (toSet dfa)
  specify "toSet (suffixes a) = setSuffixes (toSet a)" $
    property $ \(ADFA' dfa) -> toSet (suffixes dfa) === setSuffixes (toSet dfa)

  specify "uncurry fromTable . toTable = Just" $
    property $ \(ADFA' dfa) -> Just dfa === uncurry fromTable (toTable dfa)

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
