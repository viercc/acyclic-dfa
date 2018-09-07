{-| Internals for Data.ADFA -}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.ADFA.NodeId(
  NodeId(..),
) where

import           Data.Bifunctor
import           Control.DeepSeq

-- | Node s ~ Tagged s Int
newtype NodeId s = NodeId { getNodeId :: Int }
  deriving (Eq, Ord, NFData)

instance Show (NodeId s) where
  show (NodeId n) = show n

instance Read (NodeId s) where
  readsPrec p s = first NodeId <$> readsPrec p s
