{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Persistence where

import Data.Core.Graph
import Data.Core.Graph.NodeManager
import Data.Core.Graph.Persistence

import Test.Framework

prop_persistence :: NodeMap Char -> Graph -> Bool
prop_persistence nodeMap graph =
    let nodeMgr = initNodeManager nodeMap
        (nodeMgr', graph') = loadGraph (persistGraph nodeMgr graph)
    in (nodeMgr' == nodeMgr && graph == graph')
