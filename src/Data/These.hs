module Data.These(
  These(..),
  alignMap
) where

import           Data.Map              (Map)
import           Data.Map.Merge.Strict

data These a b = This a | That b | These a b
     deriving (Show, Read, Eq, Ord)

alignMap :: (Ord k) => Map k a -> Map k b -> Map k (These a b)
alignMap =
  merge (mapMissing (const This))
        (mapMissing (const That))
        (zipWithMatched (const These))
