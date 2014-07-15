{-# LANGUAGE TemplateHaskell #-}
module Data.Graph.Persistence where

import Data.Graph
import Data.Graph.NodeManager

import Data.Hashable
import Data.SafeCopy
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as VU

data PersistentGraph k
   = PersistentGraph
   { pg_nodeData :: NodeMap k
   , pg_graphData :: [(Node, [Node])]
   }

persistGraph :: (Eq k, Hashable k) => NodeManager k -> Graph -> PersistentGraph k
persistGraph nodeManager graph =
    PersistentGraph
    { pg_nodeData = nm_nodeToKey nodeManager
    , pg_graphData = map (\(k, vals) -> (k, VU.toList vals)) (IM.toList $ g_adj graph)
    }

loadGraph :: (Eq k, Hashable k) => PersistentGraph k -> (NodeManager k, Graph)
loadGraph (PersistentGraph nodeData graphData) =
    (initNodeManager nodeData, fromAdj graphData)

$(deriveSafeCopy 1 'base ''PersistentGraph)
$(deriveSafeCopy 1 'base ''Edge)
