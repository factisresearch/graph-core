{-# LANGUAGE TemplateHaskell #-}
module Data.Graph.Persistence where

import Data.Graph
import Data.Graph.NodeManager

import Data.Hashable
import Data.SafeCopy

data PersistentGraph k
   = PersistentGraph
   { pg_nodeData :: NodeMap k
   , pg_graphData :: [Edge]
   }

persistGraph :: (Eq k, Hashable k) => NodeManager k -> Graph -> PersistentGraph k
persistGraph nodeManager graph =
    PersistentGraph
    { pg_nodeData = nm_idxToNode nodeManager
    , pg_graphData = edgesAdj $ g_adj graph
    }

loadGraph :: (Eq k, Hashable k) => PersistentGraph k -> (NodeManager k, Graph)
loadGraph (PersistentGraph nodeData graphData) =
    (initNodeManager nodeData, fromEdges graphData)

$(deriveSafeCopy 1 'base ''PersistentGraph)
$(deriveSafeCopy 1 'base ''Edge)
