{-| Internals for Data.ADFA -}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.ADFA.NodeId(
  NodeId(..),
) where

import           Control.DeepSeq

-- | Node s ~ Tagged s Int
newtype NodeId s = NodeId { getNodeId :: Int }
  deriving (Eq, Ord, NFData)

instance Show (NodeId s) where
  show (NodeId n) = show n
