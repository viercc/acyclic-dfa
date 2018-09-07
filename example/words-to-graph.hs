#!/usr/bin/env stack
-- stack runghc
module Main(main) where

import           Control.Exception  (evaluate)
import           System.Environment
import           System.IO

import           Data.ADFA
import           Data.ADFA.Internal
import qualified Data.ADFA.IdVector as IV
import           Data.List
import qualified Data.Map           as Map

import           Data.Bifunctor (second)

main :: IO ()
main =
  do str <- withWordsFile $ \handle ->
       do s <- hGetContents handle
          _ <- evaluate $ length s
          return s
     let dfa = topSort . minify . strings $ lines str
     putStrLn $ printAsDot (:[]) dfa
     return ()

withWordsFile :: (Handle -> IO a) -> IO a
withWordsFile f =
  do args <- getArgs
     case args of
       []       -> f stdin
       (file:_) -> withFile file ReadMode f

printAsDot :: (c -> String) -> ADFA c -> String
printAsDot showC dfa = withInternals dfa printAsDot'
  where
    printAsDot' root nodesVec =
      let nodes = map (second isAccepted) $ IV.toAssoc nodesVec
          edges = do (x,v) <- IV.toAssoc nodesVec
                     (c,y) <- Map.toAscList (outEdges v)
                     return (x,c,y)
      in mkDigraph (nodeStmts root nodes ++ edgeStmts edges)
    
    mkDigraph stmts = unlines $
      "digraph {" : map ("  " ++) stmts ++ ["}"]
    
    nodeName n = "n" ++ show n
    
    nodeStmts root nodes =
      let (acceptedNodes, rejectedNodes) = partition snd $
            filter ((/= root) . fst) nodes
          [(_,acceptsRoot)] = filter ((==root) . fst) nodes
          def1 (n,_) = nodeName n ++ " [label=\"" ++ show n ++ "\"];"
          rootNumLine = if acceptsRoot then 2 else 1 :: Int
          rootAttr = "node [shape=box, peripheries=" ++ show rootNumLine ++ "];"
      in rootAttr : def1 (root, acceptsRoot) :
         "node [shape=circle];" : map def1 rejectedNodes ++
         "node [shape=circle, peripheries=2];" : map def1 acceptedNodes
    
    edgeStmts edges =
      let def1 (n, c, n') = nodeName n ++ " -> " ++ nodeName n' ++
                            " [label=\"" ++ showC c ++ "\"];"
      in map def1 edges
